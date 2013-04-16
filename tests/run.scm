(use yaml test)

(test-begin "yaml")

(define (yaml-exp yaml)
  (yaml-parse yaml
              (lambda (enc seed) (append seed (list (list 'stream-start enc))))
              (lambda (seed) (append seed (list (list 'stream-end))))
              '()))

(test-group "parsing"
  (test "stream-start" 'stream-start (caar (yaml-exp "--- foo"))))

(test-end)
(test-exit)
