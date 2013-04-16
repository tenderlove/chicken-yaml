(use yaml test srfi-1)

(test-begin "yaml")

(define (yaml-exp yaml)
  (yaml-parse yaml
              (lambda (enc seed) (append seed (list (list 'stream-start enc))))
              (lambda (seed) (append seed (list (list 'stream-end))))
              (lambda (version tags seed) (append seed (list (list 'document-start version tags))))
              (lambda (implicit? seed) (append seed (list (list 'document-end implicit?))))
              (lambda (alias seed) (append seed (list (list 'alias alias))))
              '()))

(define (find-event event-name)
  (lambda (event) (eq? event-name (car event))))

(test-group "stream"
  (test "start" 'stream-start (caar (yaml-exp "--- foo"))))

(test-group "document-start"
  (test "version" (list 'document-start '(1 1) '())
                    (find (find-event 'document-start)
                          (yaml-exp "%YAML 1.1\n--- foo")))
  (test "no version" (list 'document-start '() '())
                    (find (find-event 'document-start)
                          (yaml-exp "--- foo")))
  (test "tags" (list 'document-start '() (list (cons "!" "tag:tenderlovemaking.com,2009:")))
                    (find (find-event 'document-start)
                          (yaml-exp "%TAG ! tag:tenderlovemaking.com,2009:\n--- foo")))
)

(test-group "document-end"
  (test "implicit" '(document-end #t)
                    (find (find-event 'document-end)
                        (yaml-exp "--- foo")))
  (test "explicit" '(document-end #f)
                    (find (find-event 'document-end)
                        (yaml-exp "--- foo\n...")))
)

(test-group "alias"
  (test "A" '(alias "A") (find (find-event 'alias)
                               (yaml-exp "---\n- &A foo\n- *A"))))

(test-end)
(test-exit)
