(import scheme)
(import (chicken base))
(import (chicken file))
(import (chicken format))
(import (chicken io))
(import (chicken process))
(import (chicken process-context))
(import (chicken string))

(define (fail format-string #!rest args)
  (apply fprintf (current-error-port) (string-append format-string "\n") args)
  (exit 1))

(define (library-flags var command fallback)
  (or (get-environment-variable var)
      (begin
        (when (not (zero? (system "pkg-config --version >/dev/null")))
          (fail "Use `export ~a='~a'` or install `pkg-config`." var fallback))
        (let ((exit (system (string-append command " >/dev/null")))
              (output (with-input-from-pipe command (cut read-string #f))))
          (when (not (zero? exit))
            (fail "`~a` failed with exit code ~a." command exit))
          (when (eof-object? output)
            (fail "`~a` didn't produce any output." command))
          (string-chomp output)))))

(define csc (get-environment-variable "CHICKEN_CSC"))
(define libyaml-cflags (library-flags "LIBYAML_CFLAGS" "pkg-config --cflags yaml-0.1" ""))
(define libyaml-ldlibs (library-flags "LIBYAML_LDLIBS" "pkg-config --libs yaml-0.1" "-lyaml"))

(define args (list csc libyaml-cflags libyaml-ldlibs))
(define cmdline
  (string-append (apply format "~a -C ~a -L ~a " (map qs args))
                 (string-intersperse (map qs (command-line-arguments)) " ")))

(print cmdline)
(system* cmdline)
