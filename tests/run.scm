(import scheme)
(cond-expand
 (chicken-4
  (use yaml test srfi-1 sql-null posix))
 (chicken-5
  (import yaml test srfi-1 sql-null)
  (import (chicken file posix))
  (import (chicken process))
  (import (chicken io))
  (import (chicken condition))
  (import (chicken port))))

(test-begin "yaml")

(define (call-with-read-pipe str cb)
  (let-values (((in-fd out-fd) (create-pipe)))
              (let ((input (open-input-file* in-fd))
                    (output (open-output-file* out-fd)))
                   (with-output-to-port output (lambda () (write-string str)))
                   (close-output-port output)
                   (let ((result (cb input)))
                     (close-input-port input)
                     result))))

(define (call-with-write-pipe cb)
  (let-values (((in-fd out-fd) (create-pipe)))
              (let ((output (open-output-file* out-fd)))
                   (cb output)
                   (close-output-port output)
                   (let* ((input (open-input-file* in-fd))
                          (str (read-string #f input)))
                     (close-input-port input)
                     str))))

(define (yaml-exception yaml)
  (call-with-current-continuation
    (lambda (k)
      (with-exception-handler (lambda (x) (k x))
                              (lambda () (yaml-load yaml))))))

(define (find-event event-name events)
  (find (lambda (event) (eq? event-name (car event))) events))

(test-group "stream"
  (test "start" (list 'stream-start 1)
                (find-event 'stream-start (yaml-parse-events "--- foo"))))

(test-group "document-start"
  (test "version" (list 'document-start '(1 1) '() #f)
                    (find-event 'document-start
                          (yaml-parse-events "%YAML 1.1\n--- foo")))
  (test "no version" (list 'document-start '() '() #f)
                    (find-event 'document-start
                          (yaml-parse-events "--- foo")))
  (test "tags" (list 'document-start '() (list (cons "!" "tag:tenderlovemaking.com,2009:")) #f)
                    (find-event 'document-start
                          (yaml-parse-events "%TAG ! tag:tenderlovemaking.com,2009:\n--- foo")))
)

(test-group "document-end"
  (test "implicit" '(document-end #t)
                    (find-event 'document-end (yaml-parse-events "--- foo")))
  (test "explicit" '(document-end #f)
                    (find-event 'document-end (yaml-parse-events "--- foo\n...")))
)

(test-group "alias"
  (test "A" '(alias "A") (find-event 'alias (yaml-parse-events "---\n- &A foo\n- *A"))))

(test-group "scalar"
  (test "plain" '(scalar "foo" #f #f #t #f 1)
                (find-event 'scalar (yaml-parse-events "--- foo")))
  (test "quoted" '(scalar "foo" #f #f #f #t 2)
                (find-event 'scalar (yaml-parse-events "--- 'foo'")))
  (test "tag" '(scalar "foo" #f "!str" #f #f 1)
                (find-event 'scalar (yaml-parse-events "--- !str foo")))
  (test "alias" '(scalar "foo" "A" #f #t #f 1)
                (find-event 'scalar (yaml-parse-events "--- &A foo")))
)

(test-group "sequence-start"
  (test "start" '(sequence-start #f #f #t 1)
                (find-event 'sequence-start (yaml-parse-events "---\n- foo")))
  (test "tag" '(sequence-start #f "tag:yaml.org,2002:seq" #f 2)
                (find-event 'sequence-start (yaml-parse-events "!!seq [ 'foo' ]")))
  (test "anchor" '(sequence-start "1" #f #t 1)
                (find-event 'sequence-start (yaml-parse-events "--- &1\n- 1\n")))
  (test "style" '(sequence-start #f #f #t 2)
                (find-event 'sequence-start (yaml-parse-events "[ 'foo' ]")))
)

(test-group "sequence-end"
  (test "end" '(sequence-end)
                (find-event 'sequence-end (yaml-parse-events "[ 'foo' ]")))
)

(test-group "mapping-start"
  (test "start" '(mapping-start #f #f #t 1)
                (find-event 'mapping-start (yaml-parse-events "---\nfoo: bar")))
  (test "tag" '(mapping-start #f "tag:yaml.org,2002:map" #f 2)
                (find-event 'mapping-start (yaml-parse-events "!!map { foo: bar }")))
  (test "anchor" '(mapping-start "A" #f #t 2)
                (find-event 'mapping-start (yaml-parse-events "--- &A { foo: bar }")))
  (test "style" '(mapping-start #f #f #t 2)
                (find-event 'mapping-start (yaml-parse-events "{ foo: bar }")))
)

(test-group "mapping-end"
  (test "end" '(mapping-end)
                (find-event 'mapping-end (yaml-parse-events "{ foo: bar }")))
)

(define (round-trip-doc doc)
  (let ((events (yaml-parse-events doc)))
    (call-with-write-pipe (lambda (port)
               (with-yaml-emitter port (lambda (emitter)
                 (for-each (lambda (event)
                             (let ((fun (eval (car event)))
                                   (args (cons emitter (cdr event))))
                               (apply fun args)))
                           events)))))))

(test-group "event round trip"
  (let ((doc "--- &1\n- *1\n"))
    (test doc (round-trip-doc doc)))
  (let ((doc "--- foo\n...\n"))
    (test doc (round-trip-doc doc)))
  (let ((doc "--- [foo]\n...\n"))
    (test doc (round-trip-doc doc)))
  (let ((doc "--- {a: b}\n...\n"))
    (test doc (round-trip-doc doc)))
  (let ((doc "%YAML 1.1\n--- foo\n...\n"))
    (test doc (round-trip-doc doc))))


(test-group "load"
  (test-group "error"
    (test-error (yaml-load "--- ["))
    (let ((exn (yaml-exception "--- `")))
      (test-assert (get-condition-property exn 'exn 'message))
      (test-assert (get-condition-property exn 'exn 'problem))
      (test-assert (get-condition-property exn 'exn 'context))
      (test-assert (get-condition-property exn 'exn 'line))
      (test-assert (get-condition-property exn 'exn 'column))))


  (test-group "string"
    (test "foo" (yaml-load "--- foo"))
    (test 1 (yaml-load "--- 1"))
    (test 1.2 (yaml-load "--- 1.2"))
    (test 1000 (yaml-load "--- 1,000"))
    (test 1000 (yaml-load "--- 1_000"))
    (test 1000.0 (yaml-load "--- 1_000.0"))
    (test -inf.0 (yaml-load "--- -.inf"))
    (test +inf.0 (yaml-load "--- .inf"))
    (test +inf.0 (yaml-load "--- +.inf"))
    (test #t (yaml-load "--- true"))
    (test #t (yaml-load "--- True"))
    (test #t (yaml-load "--- yEs"))
    (test #t (yaml-load "--- oN"))
    (test #f (yaml-load "--- false"))
    (test #f (yaml-load "--- False"))
    (test #f (yaml-load "--- No"))
    (test #f (yaml-load "--- oFf"))
    (test-assert (sql-null? (yaml-load "--- ")))
    (test-assert
      (let ((value (yaml-load "--- .nan")))
        (not (= value value))))
    (test "1.2" (yaml-load "--- '1.2'"))
    (test 'foo (yaml-load "--- :foo")))

  (test-group "list"
    (let* ((l '())
           (lst (list l l)))
      (test lst (yaml-load "---\n- &1 []\n- *1\n")))
    (test (list "foo" "bar") (yaml-load "--- ['foo', 'bar']"))
    (test (list "foo" (list "bar")) (yaml-load "--- ['foo', ['bar']]")))

  (test-group "hash"
    (let* ((h (list (cons "a" "b")))
           (lst (list h h)))
      (test lst (yaml-load "---\n- &1 {a: b}\n- *1\n")))
    (test '(("x" . "a") ("y" . "a"))
	  (yaml-load "---\nx: &key a\ny: *key\n"))
    (test (list (cons "foo" "bar")) (yaml-load "--- {'foo':'bar'}"))
    (test (list (list "foo" (cons "bar" "baz")))
          (yaml-load "--- {'foo':{'bar':'baz'}}")))

  (test-group "port"
    (test (list "foo") (call-with-read-pipe "--- [foo]" yaml-load))))

(define (test-roundtrip object)
  (test object (yaml-load (yaml-dump object))))

(test-group "dump"
  (test-group "events"
    (let ((yaml (call-with-write-pipe (lambda (port)
                  (with-yaml-emitter port (lambda (emitter)
                    (stream-start emitter yaml:utf8-encoding)
                    (document-start emitter (list 1 1) '() #f)
                    (scalar emitter "foo" #f #f #t #f yaml:scalar-style:any)
                    (document-end emitter #t)
                    (stream-end emitter)))))))
      (test "foo" (yaml-load yaml)))
    (let ((yaml (call-with-write-pipe (lambda (port)
                  (with-yaml-emitter port (lambda (emitter)
                    (stream-start emitter yaml:utf8-encoding)
                    (document-start emitter (list 1 1) '() #f)
                    (sequence-start emitter #f #f #t yaml:sequence-style:any)
                    (scalar emitter "foo" #f #f #t #f yaml:scalar-style:any)
                    (sequence-end emitter)
                    (document-end emitter #t)
                    (stream-end emitter)))))))
      (test (list "foo") (yaml-load yaml)))
    (let ((yaml (call-with-write-pipe (lambda (port)
                  (with-yaml-emitter port (lambda (emitter)
                    (stream-start emitter yaml:utf8-encoding)
                    (document-start emitter (list 1 1) '() #f)
                    (mapping-start emitter #f #f #t yaml:mapping-style:any)
                    (scalar emitter "foo" #f #f #t #f yaml:scalar-style:any)
                    (scalar emitter "bar" #f #f #t #f yaml:scalar-style:any)
                    (mapping-end emitter)
                    (document-end emitter #t)
                    (stream-end emitter)))))))
      (test (list (cons "foo" "bar")) (yaml-load yaml))))

  (test-roundtrip "foo")
  (test-roundtrip "1.2")
  (test-roundtrip (list "1.2" "foo"))
  (test-roundtrip '())
  (test-roundtrip (list (cons "foo" "bar") (cons "baz" "omg")))
  (test-roundtrip (list (list "foo") "o" "m"))
  (test-roundtrip (sql-null))
  (test-roundtrip (list 'foo))
  (test-roundtrip (list (string->symbol "")))
  (test-roundtrip (string->symbol ""))
  (test-roundtrip 1.2)
  (test-roundtrip 12)
  (test-roundtrip #t)
  (test-roundtrip #f)
  )

(test-end)
(test-exit)
