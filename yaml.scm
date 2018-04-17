;;;; yaml.scm
;;;; Bindings to libyaml

(module yaml
  (yaml-parse yaml-parse-events yaml-load yaml-dump

   ; event based emitting
   with-yaml-emitter document-start document-end scalar stream-start stream-end
   sequence-start sequence-end mapping-start mapping-end alias

   ; constants
   yaml:utf8-encoding yaml:mapping-style:any yaml:mapping-style:block
   yaml:mapping-style:flow yaml:sequence-style:any yaml:sequence-style:block
   yaml:sequence-style:flow yaml:scalar-style:any yaml:scalar-style:plain
   yaml:scalar-style:single-quoted yaml:scalar-style:double-quoted
   yaml:scalar-style:folded
  )

(import scheme chicken foreign irregex)
(use irregex srfi-13 srfi-69 lolevel sql-null posix utils)

(foreign-declare "#include <yaml.h>")

;;;; Emitter

(define (yaml-dump object #!optional port)
  (if port
      (yaml-dump-port object port)
      (let-values (((in-fd out-fd) (create-pipe)))
        (let ((input (open-input-file* in-fd))
              (output (open-output-file* out-fd)))
          (yaml-dump-port object output)
          (close-output-port output)
          (let ((result (read-all input)))
            (close-input-port input)
            result)))))

(define (emit-string emitter str)
  (cond ((string-index str #\n)
         (scalar emitter str #f #f #t #t yaml:scalar-style:literal))
        ((irregex-search '(seq bos (~ alnum)) str)
         (scalar emitter str #f #f #t #t yaml:scalar-style:double-quoted))
        ((not (eq? str (parse-scalar str)))
         (scalar emitter str #f #f #t #t yaml:scalar-style:single-quoted))
        (else
         (scalar emitter str #f #f #t #t yaml:scalar-style:plain))))

(define (walk-objects object emitter)
  (cond ((string? object) (emit-string emitter object))
        ((list? object)
         (if (and (not (null? object))
                  (pair? (car object))
                  (not (list? (car object))))
             (begin
               (mapping-start emitter #f #f #t yaml:sequence-style:any)
               (for-each (lambda (pair)
                           (walk-objects (car pair) emitter)
                           (walk-objects (cdr pair) emitter)) object)
               (mapping-end emitter))
             (begin
               (sequence-start emitter #f #f #t yaml:sequence-style:any)
               (for-each (lambda (obj) (walk-objects obj emitter)) object)
               (sequence-end emitter))))
        ((sql-null? object)
         (scalar emitter "" #f "tag:yaml.org,2002:null" #t #f yaml:scalar-style:any))
        ((symbol? object)
         (let ((str (symbol->string object)))
           (if (= 0 (string-length str))
               (scalar emitter "" #f "!scheme/symbol" #f #f yaml:scalar-style:any)
               (scalar emitter (string-append ":" str) #f #f #t #f yaml:scalar-style:any))))
        ((number? object)
         (scalar emitter (number->string object) #f #f #t #f yaml:scalar-style:any))
        ((boolean? object)
         (if object
             (scalar emitter "true" #f #f #t #f yaml:scalar-style:any)
             (scalar emitter "false" #f #f #t #f yaml:scalar-style:any)))
        (else (abort "unknown"))))

(define (yaml-dump-port object port)
  (with-yaml-emitter port (lambda (emitter)
                            (stream-start emitter yaml:utf8-encoding)
                            (document-start emitter '() '() #f)
                            (walk-objects object emitter)
                            (document-end emitter #t)
                            (stream-end emitter))))

(define-record-type emitter-context
                    (wrap-emitter context port)
                    emitter-context?
                    (context get-emitter))

(define (with-yaml-emitter port cb)
  (let ((emitter (make-yaml-emitter port)))
    (cb emitter)
    (free-emitter emitter)))

(define (make-yaml-emitter port)
  (let ((emitter (allocate (foreign-type-size "yaml_emitter_t"))))
    (yaml_emitter_initialize emitter)
    (yaml_emitter_set_unicode emitter 1)
    (yaml_emitter_set_indent emitter 2)
    (yaml_emitter_set_output_file emitter port)
    (wrap-emitter emitter port)))

(define (emit-event emitter-ctx cb)
  (let ((emitter (get-emitter emitter-ctx)))
    (let ((event (make-yaml-event)))
      (if (= 0 (cb event))
        (begin
          (free event)
          (free-emitter emitter-ctx)
          (abort "event initialization error"))
        (let ((state (yaml_emitter_emit emitter event)))
          (if (= 0 state)
              (let ((exn (make-emit-exception emitter)))
                (free event)
                (free-emitter emitter-ctx)
                (abort exn))
              emitter))))))

(define (make-emit-exception emitter)
  (let ((message (emitter->problem emitter)))
    (make-property-condition 'exn 'message message)))

(define (free-emitter emitter-ctx)
  (let ((emitter (get-emitter emitter-ctx)))
    (yaml_emitter_delete emitter)
    (free emitter)))

(define (stream-start emitter encoding)
  (emit-event emitter (lambda (event)
    (yaml_stream_start_event_initialize event encoding))))

(define (stream-end emitter)
  (emit-event emitter (lambda (event)
                        (yaml_stream_end_event_initialize event))))

(define (document-start emitter version tags implicit)
  (let ((version-directive (populate-version version)))
    (let-values (((head tail) (populate-tags tags)))
      (let ((event (make-yaml-event)))
        (if (= 0 (yaml_document_start_event_initialize
                   event
                   version-directive
                   head
                   tail
                   (if implicit 1 0)))
            (begin
              (free event)
              (free-emitter emitter)
              (abort "error initializing document"))
            (if (= 0 (yaml_emitter_emit (get-emitter emitter) event))
                (let ((exn (make-emit-exception emitter)))
                  (free event)
                  (free-emitter emitter)
                  (abort emitter))
                (begin
                  (if head (free head))
                  (if version-directive (free version-directive))
                  (free event)
                  emitter)))))))

(define (document-end emitter implicit)
  (emit-event emitter (lambda (event)
    (yaml_document_end_event_initialize event
                                        (if implicit 1 0)))))

(define (scalar emitter
                value
                anchor
                tag
                plain
                quoted
                style)
  (emit-event emitter (lambda (event)
    (yaml_scalar_event_initialize event
                                  anchor
                                  tag
                                  value
                                  (string-length value)
                                  (if plain 1 0)
                                  (if quoted 1 0)
                                  style))))

(define (sequence-start emitter anchor tag implicit style)
  (emit-event emitter (lambda (event)
    (yaml_sequence_start_event_initialize event
                                          anchor
                                          tag
                                          (if implicit 1 0)
                                          style))))

(define (sequence-end emitter)
  (emit-event emitter (lambda (event)
    (yaml_sequence_end_event_initialize event))))

(define (mapping-start emitter anchor tag implicit style)
  (emit-event emitter (lambda (event)
    (yaml_mapping_start_event_initialize event
                                          anchor
                                          tag
                                          (if implicit 1 0)
                                          style))))

(define (mapping-end emitter)
  (emit-event emitter (lambda (event)
    (yaml_mapping_end_event_initialize event))))

(define (alias emitter anchor)
  (emit-event emitter (lambda (event)
    (yaml_alias_event_initialize event anchor))))

(define (populate-tags tags)
  (if (<= 0 (length tags))
      (values #f #f)
      (abort "not yet")))

(define (populate-version version)
  (if (= 2 (length version))
    (let ((version-directive (make-yaml-version-directive)))
      (begin
        (set-version-directive.major version-directive (car version))
        (set-version-directive.minor version-directive (cadr version))
        version-directive))
    #f))

;;;; Parser

(define (yaml-parse-events yaml)
  (reverse (yaml-parse yaml
              (lambda (enc seed) (cons (list 'stream-start enc) seed))
              (lambda (seed) (cons (list 'stream-end) seed))
              (lambda (version tags implicit seed)
                      (cons (list 'document-start version tags implicit) seed))
              (lambda (implicit? seed)
                      (cons (list 'document-end implicit?) seed))
              (lambda (alias seed) (cons (list 'alias alias) seed))
              (lambda (value anchor tag plain quoted style seed)
                      (cons (list 'scalar value anchor tag plain quoted style) seed))
              (lambda (anchor tag implicit style seed)
                      (cons (list 'sequence-start anchor tag implicit style) seed))
              (lambda (seed) (cons (list 'sequence-end) seed))
              (lambda (anchor tag implicit style seed)
                      (cons (list 'mapping-start anchor tag implicit style) seed))
              (lambda (seed) (cons (list 'mapping-end) seed))
              '())))

(define (parser-sequence-end seed)
  (let loop ((the-list '()) (stack seed))
    (if (start-sequence-ctx? (car stack))
      (let* ((ctx (car stack))
             (anchor (get-sequence-anchor ctx))
             (anchors (get-sequence-anchors ctx))
             (result (cons the-list (cdr stack))))
        (if anchor (hash-table-set! anchors anchor the-list))
        result)
      (loop (cons (car stack) the-list) (cdr stack)))))

(define (parser-mapping-end seed)
  (let loop ((the-list '()) (stack seed))
    (if (start-mapping-ctx? (car stack))
      (let* ((ctx (car stack))
             (anchor (get-mapping-anchor ctx))
             (anchors (get-mapping-anchors ctx))
             (result (cons the-list (cdr stack))))
        (if anchor (hash-table-set! anchors anchor the-list))
        result)
        (loop (cons (cons (cadr stack) (car stack)) the-list) (cddr stack)))))

(define (parser-scalar value anchor tag plain quoted style seed)
  (if (equal? "!scheme/symbol" tag)
      (cons (string->symbol (irregex-replace "^:" value)) seed)
      (if quoted
          (cons value seed)
          (cons (parse-scalar value) seed))))

(define possible-string (string->irregex "[^0-9.:+-].*"))

(define (parse-scalar value)
  (cond ((string-null? value) (sql-null))
        ((irregex-match possible-string value)
         (cond ((> (string-length value) 5) value)
               ((irregex-match '(w/nocase (or "yes" "true" "on")) value) #t)
               ((irregex-match '(w/nocase (or "no" "false" "off")) value) #f)
               (else value)))
        ((string-index value #\: 0 1)
         (string->symbol (irregex-replace "^:" value)))
        ((irregex-match "[-+]?[0-9][0-9_,]*" value)
         (string->number (irregex-replace/all "[,_]" value "")))
        ((irregex-match "-?([0-9][0-9_,]*)?\\.[0-9]*([eE][-+][0-9]+)?" value)
         (string->number (irregex-replace/all "[,_]" value "")))
        ((irregex-match "[-+]?.(?:inf|INF|Inf)" value)
         (let ((str (string-append (irregex-replace/all "[.]" value "") ".0")))
           (if (irregex-search '(seq bos (or "+" "-")) str)
               (string->number str)
               +inf.0)))
        ((irregex-match "[-+]?.(?:nan|NaN|NAN)" value)
         (let ((str (string-append (irregex-replace/all "[.]" value "") ".0")))
           (if (irregex-search '(seq bos (or "+" "-")) str)
               (string->number str)
               +nan.0)))
        (else value)))

(define-record-type start-mapping-ctx
                    (wrap-start-mapping-ctx anchor anchors)
                    start-mapping-ctx?
                    (anchor get-mapping-anchor)
                    (anchors get-mapping-anchors))

(define-record-type start-sequence-ctx
                    (wrap-start-sequence-ctx anchor anchors)
                    start-sequence-ctx?
                    (anchor get-sequence-anchor)
                    (anchors get-sequence-anchors))

; Load YAML from a string or port
(define (yaml-load string-or-port)
  (let ((anchors (make-hash-table)))
    (yaml-parse string-or-port
                (lambda (enc seed) (cons 'stream-start seed))
                (lambda (seed) seed)
                (lambda (version tags implicit seed)
                        (cons 'document-start seed))
                (lambda (implicit? seed) (car seed))
                (lambda (alias seed)
		  (cons (hash-table-ref anchors alias) seed))
                parser-scalar
                (lambda (anchor tag implicit style seed)
                        (cons (wrap-start-sequence-ctx anchor anchors) seed))
                parser-sequence-end
                (lambda (anchor tag implicit style seed)
                        (cons (wrap-start-mapping-ctx anchor anchors) seed))
                parser-mapping-end
                '())))

(define-foreign-type yaml_parser (c-pointer "yaml_parser_t"))
(define-foreign-type yaml_parser_t (c-pointer "yaml_parser_t"))
(define-foreign-type yaml_emitter_t (c-pointer "yaml_emitter_t"))
(define-foreign-type yaml_event_t (c-pointer "yaml_event_t"))
(define-foreign-type yaml_version_directive_t (c-pointer "yaml_version_directive_t"))
(define-foreign-type yaml_tag_directive_t (c-pointer "yaml_tag_directive_t"))
(define sizeof_tag_directive_t (foreign-type-size "yaml_tag_directive_t"))

(define-record-type parser-context
                    (wrap-parser context
                                 stream-start
                                 stream-end
                                 document-start
                                 document-end
                                 alias
                                 scalar
                                 sequence-start
                                 sequence-end
                                 mapping-start
                                 mapping-end
                                 seed)
                    parser-context?
                    (context get-context)
                    (stream-start get-stream-start)
                    (stream-end get-stream-end)
                    (document-start get-document-start)
                    (document-end get-document-end)
                    (sequence-start get-sequence-start)
                    (sequence-end get-sequence-end)
                    (mapping-start get-mapping-start)
                    (mapping-end get-mapping-end)
                    (scalar get-scalar)
                    (alias get-alias)
                    (seed get-seed))

(define (yaml-parse string-or-port
                    stream-start
                    stream-end
                    document-start
                    document-end
                    alias
                    scalar
                    sequence-start
                    sequence-end
                    mapping-start
                    mapping-end
                    seed)
  (let* ((parser (make-yaml-parser))
         (ctx (wrap-parser parser
                           stream-start
                           stream-end
                           document-start
                           document-end
                           alias
                           scalar
                           sequence-start
                           sequence-end
                           mapping-start
                           mapping-end
                           seed)))
    (let ((results (do-parse string-or-port ctx)))
      (free-yaml-parser! parser)
      results)))

(define (do-parse string-or-port ctx)
  (if (input-port? string-or-port)
      (do-parse-input string-or-port ctx)
      (do-parse-string string-or-port ctx)))

(define yaml:mapping-style:any (foreign-value "YAML_ANY_MAPPING_STYLE" int))
(define yaml:mapping-style:block (foreign-value "YAML_BLOCK_MAPPING_STYLE" int))
(define yaml:mapping-style:flow (foreign-value "YAML_FLOW_MAPPING_STYLE" int))

(define yaml:sequence-style:any (foreign-value "YAML_ANY_SEQUENCE_STYLE" int))
(define yaml:sequence-style:block (foreign-value "YAML_BLOCK_SEQUENCE_STYLE" int))
(define yaml:sequence-style:flow (foreign-value "YAML_FLOW_SEQUENCE_STYLE" int))

(define yaml:scalar-style:any (foreign-value "YAML_ANY_SCALAR_STYLE" int))
(define yaml:scalar-style:plain (foreign-value "YAML_PLAIN_SCALAR_STYLE" int))
(define yaml:scalar-style:single-quoted (foreign-value "YAML_SINGLE_QUOTED_SCALAR_STYLE" int))
(define yaml:scalar-style:double-quoted (foreign-value "YAML_DOUBLE_QUOTED_SCALAR_STYLE" int))
(define yaml:scalar-style:literal (foreign-value "YAML_LITERAL_SCALAR_STYLE" int))
(define yaml:scalar-style:folded (foreign-value "YAML_FOLDED_SCALAR_STYLE" int))

(define yaml:any-encoding (foreign-value "YAML_ANY_ENCODING" int))
(define yaml:utf8-encoding (foreign-value "YAML_UTF8_ENCODING" int))
(define yaml:stream-start-event (foreign-value "YAML_STREAM_START_EVENT" int))
(define yaml:stream-end-event (foreign-value "YAML_STREAM_END_EVENT" int))
(define yaml:document-start-event (foreign-value "YAML_DOCUMENT_START_EVENT" int))
(define yaml:document-end-event (foreign-value "YAML_DOCUMENT_END_EVENT" int))
(define yaml:alias-event (foreign-value "YAML_ALIAS_EVENT" int))
(define yaml:scalar-event (foreign-value "YAML_SCALAR_EVENT" int))
(define yaml:sequence-start-event (foreign-value "YAML_SEQUENCE_START_EVENT" int))
(define yaml:sequence-end-event (foreign-value "YAML_SEQUENCE_END_EVENT" int))
(define yaml:mapping-start-event (foreign-value "YAML_MAPPING_START_EVENT" int))
(define yaml:mapping-end-event (foreign-value "YAML_MAPPING_END_EVENT" int))

(define (handle-stream-start-event ctx event seed)
  (let ((cb (get-stream-start ctx)))
    (cb (event.data.start_stream.encoding event) seed)))

(define (handle-stream-end-event ctx event seed)
  ((get-stream-end ctx) seed))

(define (handle-document-start-event ctx event seed)
  (let ((cb (get-document-start ctx)))
    (cb (document-version-directives event)
        (tag-directives event)
        (document-start-implicit event)
        seed)))

(define (handle-document-end-event ctx event seed)
  (let ((cb (get-document-end ctx)))
    (cb (not (= 0 (document-end-implicit event)))
        seed)))

(define (handle-sequence-start-event ctx event seed)
  (let ((cb (get-sequence-start ctx)))
    (cb (sequence-anchor event)
        (sequence-tag event)
        (not (= 0 (sequence-implicit event)))
        (sequence-style event)
        seed)))

(define (handle-mapping-start-event ctx event seed)
  (let ((cb (get-mapping-start ctx)))
    (cb (mapping-anchor event)
        (mapping-tag event)
        (not (= 0 (mapping-implicit event)))
        (mapping-style event)
        seed)))

(define (handle-sequence-end-event ctx event seed)
  ((get-sequence-end ctx) seed))

(define (handle-scalar-event ctx event seed)
  (let ((cb (get-scalar ctx)))
    (cb (scalar-value event)
        (scalar-anchor event)
        (scalar-tag event)
        (not (= 0 (scalar-plain-implicit event)))
        (not (= 0 (scalar-quoted-implicit event)))
        (scalar-style event)
        seed)))

(define (handle-alias-event ctx event seed)
  ((get-alias ctx) (alias-anchor event) seed))

(define (handle-mapping-end-event ctx event seed)
  ((get-mapping-end ctx) seed))

(define (make-exception ctx)
  (let ((line (_error-line ctx))
        (column (_error-column ctx))
        (problem (_error-problem ctx))
        (context (_error-context ctx)))
    (make-property-condition
      'exn
      'message (string-append
                 problem " " context " at line "
                 (number->string line) " column "
                 (number->string column))
      'line line
      'column column
      'problem problem
      'context context)))

(define (pull-event parser event)
  (let ((state (yaml_parser_parse parser event)))
    (if (= 0 state)
        (let ((exn (make-exception parser)))
          (free-yaml-event event)
          (free-yaml-parser! parser)
          (abort exn))
        state)))

(define (parse-loop ctx parser event seed)
  (let* ((state (pull-event parser event))
         (type (event.type event)))
    (cond ((= yaml:stream-start-event type)
           (parse-loop ctx parser event
                       (handle-stream-start-event ctx event seed)))

          ((= yaml:document-start-event type)
           (parse-loop ctx parser event
                       (handle-document-start-event ctx event seed)))

          ((= yaml:sequence-start-event type)
           (parse-loop ctx parser event
                       (handle-sequence-start-event ctx event seed)))

          ((= yaml:sequence-end-event type)
           (parse-loop ctx parser event
                       (handle-sequence-end-event ctx event seed)))

          ((= yaml:mapping-start-event type)
           (parse-loop ctx parser event
                       (handle-mapping-start-event ctx event seed)))

          ((= yaml:mapping-end-event type)
           (parse-loop ctx parser event
                       (handle-mapping-end-event ctx event seed)))

          ((= yaml:scalar-event type)
           (parse-loop ctx parser event
                       (handle-scalar-event ctx event seed)))

          ((= yaml:alias-event type)
           (parse-loop ctx parser event
                       (handle-alias-event ctx event seed)))

          ((= yaml:document-end-event type)
           (parse-loop ctx parser event
                       (handle-document-end-event ctx event seed)))

          ((= yaml:stream-end-event type)
           (handle-stream-end-event ctx event seed))

          (else (parse-loop ctx parser event seed)))))

(define (do-parse-string yaml ctx)
  (yaml_parser_set_input_string (get-context ctx)
                                yaml
                                (string-length yaml))
  (let ((parser (get-context ctx))
        (event (make-yaml-event)))
    (let ((seed (parse-loop ctx parser event (get-seed ctx))))
      (free-yaml-event event)
      seed)))

(define (do-parse-input yaml ctx)
  (yaml_parser_set_input_file (get-context ctx) yaml)
  (let ((parser (get-context ctx))
        (event (make-yaml-event)))
    (let ((seed (parse-loop ctx parser event (get-seed ctx))))
      (free-yaml-event event)
      seed)))

(define yaml_parser_set_input_string (foreign-lambda void
                                                     "yaml_parser_set_input_string"
                                                     yaml_parser_t
                                                     nonnull-unsigned-c-string
                                                     size_t))

(define yaml_parser_set_input_file (foreign-lambda void
                                                   "yaml_parser_set_input_file"
                                                   yaml_parser_t
                                                   c-pointer))

(define (make-yaml-version-directive)
  (allocate (foreign-type-size "yaml_version_directive_t")))

(define (make-yaml-event) (allocate (foreign-type-size "yaml_event_t")))

(define (free-yaml-event event) (yaml_event_delete event) (free event))

(define yaml_emitter_delete (foreign-lambda void
                                            "yaml_emitter_delete"
                                            yaml_emitter_t))

(define yaml_emitter_emit (foreign-lambda int
                                          "yaml_emitter_emit"
                                          yaml_emitter_t
                                          yaml_event_t))

(define yaml_document_start_event_initialize (foreign-lambda int
                                                             "yaml_document_start_event_initialize"
                                                             yaml_event_t
                                                             yaml_version_directive_t
                                                             yaml_tag_directive_t
                                                             yaml_tag_directive_t
                                                             int))

(define yaml_document_end_event_initialize (foreign-lambda int
                                                             "yaml_document_end_event_initialize"
                                                             yaml_event_t
                                                             int))

(define yaml_stream_start_event_initialize (foreign-lambda int
                                                           "yaml_stream_start_event_initialize"
                                                           yaml_event_t
                                                           int))

(define yaml_stream_end_event_initialize (foreign-lambda int
                                                         "yaml_stream_end_event_initialize"
                                                         yaml_event_t))

(define yaml_scalar_event_initialize (foreign-lambda int
                                                     "yaml_scalar_event_initialize"
                                                     yaml_event_t
                                                     unsigned-c-string
                                                     unsigned-c-string
                                                     nonnull-unsigned-c-string
                                                     int
                                                     int
                                                     int
                                                     int))

(define yaml_sequence_start_event_initialize (foreign-lambda int
                                                             "yaml_sequence_start_event_initialize"
                                                             yaml_event_t
                                                             unsigned-c-string
                                                             unsigned-c-string
                                                             int
                                                             int))

(define yaml_sequence_end_event_initialize (foreign-lambda int
                                                           "yaml_sequence_end_event_initialize"
                                                           yaml_event_t))

(define yaml_mapping_start_event_initialize (foreign-lambda int
                                                             "yaml_mapping_start_event_initialize"
                                                             yaml_event_t
                                                             unsigned-c-string
                                                             unsigned-c-string
                                                             int
                                                             int))

(define yaml_mapping_end_event_initialize (foreign-lambda int
                                                           "yaml_mapping_end_event_initialize"
                                                           yaml_event_t))


(define yaml_alias_event_initialize (foreign-lambda int
                                                    "yaml_alias_event_initialize"
                                                    yaml_event_t
                                                    nonnull-unsigned-c-string))

(define yaml_event_delete (foreign-lambda void
                                          "yaml_event_delete"
                                          yaml_event_t))

(define yaml_parser_parse (foreign-lambda int
                                          "yaml_parser_parse"
                                          yaml_parser_t
                                          yaml_event_t))

(define yaml_parser_initialize (foreign-lambda int
                                               "yaml_parser_initialize"
                                               yaml_parser_t))

(define yaml_emitter_initialize (foreign-lambda int
                                               "yaml_emitter_initialize"
                                               yaml_emitter_t))

(define yaml_emitter_set_output_file (foreign-lambda void
                                               "yaml_emitter_set_output_file"
                                               yaml_emitter_t
                                               c-pointer))

(define yaml_emitter_set_unicode (foreign-lambda void
                                               "yaml_emitter_set_unicode"
                                               yaml_emitter_t
                                               int))

(define yaml_emitter_set_indent (foreign-lambda void
                                               "yaml_emitter_set_indent"
                                               yaml_emitter_t
                                               int))

(define (make-yaml-parser)
  (let ((parser (allocate (foreign-type-size "yaml_parser_t"))))
    (yaml_parser_initialize parser)
    parser))

(define (free-yaml-parser! parser)
  (yaml_parser_delete parser)
  (free parser))

(define set-version-directive.major (foreign-lambda* void
  ((yaml_version_directive_t version)
   (int v))
  "version->major = v;"))

(define set-version-directive.minor (foreign-lambda* void
  ((yaml_version_directive_t version)
   (int v))
  "version->minor = v;"))

(define event.data.start_stream.encoding (foreign-lambda* int
  ((yaml_event_t event))
  "C_return(event->data.stream_start.encoding);"))

(define event.data.document_start.version_directive (foreign-lambda* c-pointer
  ((yaml_event_t event))
  "C_return(event->data.document_start.version_directive);"))

(define event.data.document_start.implicit (foreign-lambda* int
  ((yaml_event_t event))
  "C_return(event->data.document_start.implicit);"))

(define (document-start-implicit event)
  (= 1 (event.data.document_start.implicit event)))

(define event.data.document_start.version_directive->major (foreign-lambda* int
  ((yaml_event_t event))
  "C_return(event->data.document_start.version_directive->major);"))

(define event.data.document_start.version_directive->minor (foreign-lambda* int
  ((yaml_event_t event))
  "C_return(event->data.document_start.version_directive->minor);"))

(define (document-version-directives event)
  (if (event.data.document_start.version_directive event)
      (list (event.data.document_start.version_directive->major event)
            (event.data.document_start.version_directive->minor event))
      '()))

(define tag-directives.start (foreign-lambda*
  yaml_tag_directive_t
  ((yaml_event_t event))
  "C_return(event->data.document_start.tag_directives.start);"))

(define tag-directives.end (foreign-lambda*
  yaml_tag_directive_t
  ((yaml_event_t event))
  "C_return(event->data.document_start.tag_directives.end);"))

(define tag-handle (foreign-lambda* c-string
                                    ((yaml_tag_directive_t tag))
                                    "C_return(tag->handle);"))

(define tag-prefix (foreign-lambda* c-string
                                    ((yaml_tag_directive_t tag))
                                    "C_return(tag->prefix);"))
(define (tag-info tag)
  (cons (tag-handle tag) (tag-prefix tag)))

(define (tag-directives event)
  (let loop ((start (tag-directives.start event))
            (end (tag-directives.end event))
            (acc '()))
    (if (or (not start) (pointer=? start end))
        acc
        (loop (pointer+ start sizeof_tag_directive_t) end (cons (tag-info start) acc)))))

(define (scalar-value event)
  (let* ((len (scalar-len event))
         (str (make-string len)))
    (move-memory! (scalar-ptr event) str len)
    str))

(define scalar-len (foreign-lambda* int
                                    ((yaml_event_t event))
                                    "C_return(event->data.scalar.length);"))

(define scalar-ptr (foreign-lambda* c-pointer
                                    ((yaml_event_t event))
                                    "C_return(event->data.scalar.value);"))

(define scalar-anchor (foreign-lambda* c-string
                                       ((yaml_event_t e))
                                       "C_return(e->data.scalar.anchor);"))

(define scalar-tag (foreign-lambda* c-string
                                       ((yaml_event_t e))
                                       "C_return(e->data.scalar.tag);"))

(define scalar-plain-implicit (foreign-lambda* int
                                       ((yaml_event_t e))
                                       "C_return(e->data.scalar.plain_implicit);"))

(define scalar-quoted-implicit (foreign-lambda* int
                                       ((yaml_event_t e))
                                       "C_return(e->data.scalar.quoted_implicit);"))

(define scalar-style (foreign-lambda* int
                                       ((yaml_event_t e))
                                       "C_return(e->data.scalar.style);"))

(define document-end-implicit (foreign-lambda* int
                                       ((yaml_event_t e))
                                       "C_return(e->data.document_end.implicit);"))

(define sequence-anchor (foreign-lambda* c-string
                                       ((yaml_event_t e))
                                       "C_return(e->data.sequence_start.anchor);"))

(define sequence-tag (foreign-lambda* c-string
                                       ((yaml_event_t e))
                                       "C_return(e->data.sequence_start.tag);"))

(define sequence-implicit (foreign-lambda* int
                                       ((yaml_event_t e))
                                       "C_return(e->data.sequence_start.implicit);"))

(define sequence-style (foreign-lambda* int
                                       ((yaml_event_t e))
                                       "C_return(e->data.sequence_start.style);"))

(define mapping-anchor (foreign-lambda* c-string
                                       ((yaml_event_t e))
                                       "C_return(e->data.mapping_start.anchor);"))

(define mapping-tag (foreign-lambda* c-string
                                       ((yaml_event_t e))
                                       "C_return(e->data.mapping_start.tag);"))

(define mapping-implicit (foreign-lambda* int
                                       ((yaml_event_t e))
                                       "C_return(e->data.mapping_start.implicit);"))

(define mapping-style (foreign-lambda* int
                                       ((yaml_event_t e))
                                       "C_return(e->data.mapping_start.style);"))

(define alias-anchor (foreign-lambda* c-string
                                       ((yaml_event_t e))
                                       "C_return(e->data.alias.anchor);"))

(define yaml_parser_delete (foreign-lambda void
                                           "yaml_parser_delete"
                                            yaml_parser_t))

(define _error-line (foreign-lambda* int
                                     ((yaml_parser_t parser))
                                     "C_return(parser->context_mark.line + 1);"))

(define _error-column (foreign-lambda* int
                                     ((yaml_parser_t parser))
                                     "C_return(parser->context_mark.column + 1);"))

(define emitter->problem (foreign-lambda* c-string
                                          ((yaml_emitter_t emitter))
                                          "C_return(emitter->problem);"))

(define _error-problem (foreign-lambda* c-string
                                     ((yaml_parser_t parser))
                                     "C_return(parser->problem);"))

(define _error-context (foreign-lambda* c-string
                                     ((yaml_parser_t parser))
                                     "C_return(parser->context);"))

(define event.type (foreign-lambda* int
  ((yaml_event_t event))
  "C_return(event->type);"))
)
