(use yaml test srfi-1)

(test-begin "yaml")

(define (yaml-exp yaml)
  (yaml-parse yaml
              (lambda (enc seed) (append seed (list (list 'stream-start enc))))
              (lambda (seed) (append seed (list (list 'stream-end))))
              (lambda (version tags seed) (append seed (list (list 'document-start version tags))))
              (lambda (implicit? seed) (append seed (list (list 'document-end implicit?))))
              '()))

(test-group "parsing"
  (test "stream-start" 'stream-start (caar (yaml-exp "--- foo")))
  (test "doc-start" (list 'document-start '(1 1) '())
                    (find (lambda (event) (eq? 'document-start (car event)))
                          (yaml-exp "%YAML 1.1\n--- foo")))
  (test "doc-start no version" (list 'document-start '() '())
                    (find (lambda (event) (eq? 'document-start (car event)))
                          (yaml-exp "--- foo")))
  (test "doc-start tags" (list 'document-start '() (list (cons "!" "tag:tenderlovemaking.com,2009:")))
                    (find (lambda (event) (eq? 'document-start (car event)))
                          (yaml-exp "%TAG ! tag:tenderlovemaking.com,2009:\n--- foo")))
  (test "doc-end implicit" '(document-end #t)
                  (find (lambda (event) (eq? 'document-end (car event)))
                        (yaml-exp "--- foo")))
  (test "doc-end explicit" '(document-end #f)
                  (find (lambda (event) (eq? 'document-end (car event)))
                        (yaml-exp "--- foo\n...")))
)

(test-end)
(test-exit)
