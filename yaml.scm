;;;; yaml.scm
;;;; Bindings to libyaml

(module yaml
  (yaml-parse
  yaml-load
  yaml-parse2)

(import scheme chicken foreign irregex)
(use irregex srfi-13)

(foreign-declare "#include <yaml.h>")


(define (sequence-end seed)
  (let loop ((the-list '()) (stack seed))
    (if (eq? 'sequence-start (car stack))
      (cons the-list (cdr stack))
      (loop (cons (car stack) the-list) (cdr stack)))))

(define (mapping-end seed)
  (let loop ((the-list '()) (stack seed))
    (if (eq? 'mapping-start (car stack))
        (cons the-list (cdr stack))
        (loop (cons (cons (cadr stack) (car stack)) the-list) (cddr stack)))))

(define (scalar value anchor tag plain quoted style seed)
  (cons (parse-scalar value) seed))

(define (parse-scalar value)
  (cond ((irregex-match "[-+]?[0-9]+" value) (string->number value))
        ((irregex-match "[-+]?([0-9][0-9_,]*)?\.[0-9]*([eE][-+][0-9]+)?|[+-]?" value)
         (string->number (irregex-replace/all "[,_]" value "")))
        ((irregex-match "[-+]?.(?:inf|INF|Inf)" value)
         (string->number (irregex-replace/all "[.]" value "")))
        ((irregex-match "[-+]?.(?:nan|NaN|NAN)" value)
         (string->number (irregex-replace/all "[.]" value "")))
        (else value)))

(define (yaml-load yaml)
  (yaml-parse yaml
              (lambda (enc seed) (cons 'stream-start seed))
              (lambda (seed) seed)
              (lambda (version tags seed)
                      (cons 'document-start seed))
              (lambda (implicit? seed) (car seed))
              (lambda (alias seed) seed)
              scalar
              (lambda (anchor tag implicit style seed)
                      (cons 'sequence-start seed))
              sequence-end
              (lambda (anchor tag implicit style seed)
                      (cons 'mapping-start seed))
              mapping-end
              '()))

(define-foreign-type yaml_parser (c-pointer "yaml_parser_t"))
(define-foreign-type yaml_parser_t (c-pointer "yaml_parser_t"))
(define-foreign-type yaml_event_t (c-pointer "yaml_event_t"))

(define (add-tag pair tags) (append tags (list pair)))

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
                    (seed get-seed))

(define (yaml-parse2 yaml
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
    (do-parse yaml ctx)))

(define yaml:stream-start-event (foreign-value "YAML_STREAM_START_EVENT" int))
(define yaml:stream-end-event (foreign-value "YAML_STREAM_END_EVENT" int))
(define yaml:document-start-event (foreign-value "YAML_DOCUMENT_START_EVENT" int))

(define (handle-stream-start-event ctx event seed)
  (let ((cb (get-stream-start ctx)))
    (cb (event.data.start_stream.encoding event) seed)))

(define (handle-stream-end-event ctx event seed)
  ((get-stream-end ctx) seed))

(define (handle-document-start-event ctx event seed)
  (let ((cb (get-document-start ctx)))
    (cb (event.data.document_start.version_directives event) seed)))

(define (do-parse yaml ctx)
  (yaml_parser_set_input_string (get-context ctx)
                                yaml
                                (string-length yaml))
  (let ((parser (get-context ctx))
        (event (make-yaml-event)))
    (let loop ((seed (get-seed ctx))
               (state (yaml_parser_parse parser event)))
    (cond ((= yaml:stream-start-event state)
           (loop (handle-stream-start-event ctx event seed)
                 (yaml_parser_parse parser event)))

          ((= yaml:document-start-event state)
           (loop (handle-document-start-event ctx event seed)
                 (yaml_parser_parse parser event)))

          ((= yaml:stream-end-event)
           (handle-stream-end-event ctx event seed))))))

(define (yaml-parse yaml
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
  (parse_yaml yaml
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
              seed
              add-tag
              make-exception
              abort))

(define (make-exception line column offset problem context)
  (let ((c (if context context "omg"))
        (p (if problem problem "zomg")))
    (make-property-condition
         'yaml-parse-error
         'message (string-append "(<unknown>): "
                    p " " c " at line "
                    (number->string line) " column "
                    (number->string column)))))

(define yaml_parser_set_input_string (foreign-lambda void
                                                     "yaml_parser_set_input_string"
                                                     yaml_parser_t
                                                     nonnull-unsigned-c-string
                                                     size_t))
(define parse_yaml (foreign-safe-lambda* void
                                         ((nonnull-unsigned-c-string yaml)
                                          (scheme-object stream_start)
                                          (scheme-object stream_end)
                                          (scheme-object document_start)
                                          (scheme-object document_end)
                                          (scheme-object alias)
                                          (scheme-object scalar)
                                          (scheme-object sequence_start)
                                          (scheme-object sequence_end)
                                          (scheme-object mapping_start)
                                          (scheme-object mapping_end)
                                          (scheme-object seed)
                                          (scheme-object add_tag)
                                          (scheme-object make_exception)
                                          (scheme-object abort_fn)
                                          )
    "yaml_parser_t * parser;
    int done = 0;
    int state = 0;
    yaml_event_t event;
    parser = malloc(sizeof(yaml_parser_t));
    yaml_parser_initialize(parser);
    yaml_parser_set_input_string(
      parser,
      (const unsigned char *)(yaml),
      (size_t)strlen((const char *)yaml)
    );
    while(!done) {
      if(!yaml_parser_parse(parser, &event)) {
        C_word *_problem;
        C_word problem = C_SCHEME_FALSE;
        C_word *_context;
        C_word context = C_SCHEME_FALSE;
        C_word exception;

        if (parser->problem) {
          _problem =
            C_alloc(C_SIZEOF_STRING(strlen((const char *)parser->problem)));
          problem = C_string2(&_problem, (char *)parser->problem);
        }

        if (parser->context) {
          _context =
            C_alloc(C_SIZEOF_STRING(strlen((const char *)parser->context)));
          context = C_string2(&_context, (char *)parser->context);
        }

        C_save(C_fix(parser->context_mark.line + 1));
        C_save(C_fix(parser->context_mark.column + 1));
        C_save(C_fix(parser->problem_offset));
        C_save(problem);
        C_save(context);
        exception = C_callback(make_exception, 5);

        yaml_parser_delete(parser);
        free(parser);
        done = 1;
        C_save(exception);
        C_callback(abort_fn, 1);
        C_return(seed);
      }
      switch(event.type) {
        case YAML_STREAM_START_EVENT: {
            C_save(C_fix(event.data.stream_start.encoding));
            C_save(seed);
            seed = C_callback(stream_start, 2);
          }
          break;
        case YAML_DOCUMENT_START_EVENT: {
            C_word tags;
            C_word version;

            tags = C_list(NULL, 0);

            if (event.data.document_start.version_directive) {
              C_word * versionlist = C_alloc(C_SIZEOF_LIST(2));
              version = C_list(&versionlist, 2,
                                C_fix(event.data.document_start.version_directive->major),
                                C_fix(event.data.document_start.version_directive->minor));
            } else {
              version = C_list(NULL, 0);
            }

            if (event.data.document_start.tag_directives.start) {
              yaml_tag_directive_t *start =
                event.data.document_start.tag_directives.start;
              yaml_tag_directive_t *end =
                event.data.document_start.tag_directives.end;

              for(; start != end; start++) {
                C_word handle = C_SCHEME_FALSE;
                C_word prefix = C_SCHEME_FALSE;
                C_word *p  = C_alloc(C_SIZEOF_PAIR);

                C_word pair;
                if (start->handle) {
                  C_word *_handle = C_alloc(C_SIZEOF_STRING(strlen((const char *)start->handle)));
                  handle = C_string2(&_handle, (char *)start->handle);
                }

                if (start->prefix) {
                  C_word *_prefix = C_alloc(C_SIZEOF_STRING(strlen((const char *)start->prefix)));
                  prefix = C_string2(&_prefix, (char *)start->prefix);
                }

                pair = C_pair(&p, handle, prefix);
                C_save(pair);
                C_save(tags);
                tags = C_callback(add_tag, 2);
              }
            }

            C_save(version);
            C_save(tags);
            C_save(seed);
            seed = C_callback(document_start, 3);
          }
          break;
        case YAML_DOCUMENT_END_EVENT: {
            C_word implicit_p;

            if (event.data.document_end.implicit) {
              implicit_p = C_SCHEME_TRUE;
            } else {
              implicit_p = C_SCHEME_FALSE;
            }

            C_save(implicit_p);
            C_save(seed);
            seed = C_callback(document_end, 2);
          }
          break;
        case YAML_ALIAS_EVENT: {
            C_word alias_str = C_SCHEME_FALSE;
            C_word *a;

            if (event.data.alias.anchor) {
              a = C_alloc(C_SIZEOF_STRING(strlen((const char *)event.data.alias.anchor)));
              alias_str = C_string2(&a, (char *)event.data.alias.anchor);
            }

            C_save(alias_str);
            C_save(seed);
            seed = C_callback(alias, 2);
          }
          break;
        case YAML_SCALAR_EVENT: {
            C_word anchor = C_SCHEME_FALSE;
            C_word tag = C_SCHEME_FALSE;
            C_word plain_implicit, quoted_implicit, style;
            C_word val;
            C_word *a;
            a = C_alloc(C_SIZEOF_STRING(event.data.scalar.length));
            val = C_string(&a, event.data.scalar.length, (char *)event.data.scalar.value);

            if (event.data.scalar.anchor) {
              C_word *a = C_alloc(C_SIZEOF_STRING(strlen((const char *)event.data.scalar.anchor)));
              anchor = C_string2(&a, (char *)event.data.scalar.anchor);
            }

            if (event.data.scalar.tag) {
              C_word *a = C_alloc(C_SIZEOF_STRING(strlen((const char *)event.data.scalar.tag)));
              tag = C_string2(&a, (char *)event.data.scalar.tag);
            }

            plain_implicit =
              event.data.scalar.plain_implicit == 0 ? C_SCHEME_FALSE : C_SCHEME_TRUE;

            quoted_implicit =
              event.data.scalar.quoted_implicit == 0 ? C_SCHEME_FALSE : C_SCHEME_TRUE;

            style = C_fix(event.data.scalar.style);

            C_save(val);
            C_save(anchor);
            C_save(tag);
            C_save(plain_implicit);
            C_save(quoted_implicit);
            C_save(style);
            C_save(seed);
            seed = C_callback(scalar, 7);
          }
          break;
        case YAML_SEQUENCE_START_EVENT: {
            C_word anchor = C_SCHEME_FALSE;
            C_word tag = C_SCHEME_FALSE;
            C_word implicit, style;

            if (event.data.sequence_start.anchor) {
              C_word *a = C_alloc(C_SIZEOF_STRING(strlen((const char *)event.data.sequence_start.anchor)));
              anchor = C_string2(&a, (char *)event.data.sequence_start.anchor);
            }

            if (event.data.sequence_start.tag) {
              C_word *a = C_alloc(C_SIZEOF_STRING(strlen((const char *)event.data.sequence_start.tag)));
              anchor = C_string2(&a, (char *)event.data.sequence_start.tag);
            }

            implicit = event.data.sequence_start.implicit == 0 ?
              C_SCHEME_FALSE : C_SCHEME_TRUE;

            style = C_fix(event.data.sequence_start.style);

            C_save(anchor);
            C_save(tag);
            C_save(implicit);
            C_save(style);
            C_save(seed);
            seed = C_callback(sequence_start, 5);
          }
          break;
        case YAML_SEQUENCE_END_EVENT: {
            C_save(seed);
            seed = C_callback(sequence_end, 1);
          }
          break;
        case YAML_MAPPING_START_EVENT: {
            C_word anchor = C_SCHEME_FALSE;
            C_word tag = C_SCHEME_FALSE;
            C_word implicit, style;

            if (event.data.mapping_start.anchor) {
              C_word *a = C_alloc(C_SIZEOF_STRING(strlen((const char *)event.data.mapping_start.anchor)));
              anchor = C_string2(&a, (char *)event.data.mapping_start.anchor);
            }

            if (event.data.mapping_start.tag) {
              C_word *a = C_alloc(C_SIZEOF_STRING(strlen((const char *)event.data.mapping_start.tag)));
              anchor = C_string2(&a, (char *)event.data.mapping_start.tag);
            }

            implicit = event.data.mapping_start.implicit == 0 ?
              C_SCHEME_FALSE : C_SCHEME_TRUE;

            style = C_fix(event.data.mapping_start.style);

            C_save(anchor);
            C_save(tag);
            C_save(implicit);
            C_save(style);
            C_save(seed);
            seed = C_callback(mapping_start, 5);
          }
          break;
        case YAML_MAPPING_END_EVENT: {
            C_save(seed);
            seed = C_callback(mapping_end, 1);
          }
          break;
        case YAML_STREAM_END_EVENT:
          C_save(seed);
          seed = C_callback(stream_end, 1);
          done = 1;
          break;
        case YAML_NO_EVENT:
          break;
      }
      yaml_event_delete(&event);
    }
    yaml_parser_delete(parser);
    free(parser);
    C_return(seed);
    "
))

(define (make-yaml-parser)
  (set-finalizer! (alloc-yaml-parser) free-yaml-parser))

(define (make-yaml-event)
  (set-finalizer! (alloc-yaml-event (foreign-type-size "yaml_event_t"))
                  free-yaml-event))

(define alloc-yaml-event (foreign-lambda yaml_event_t "malloc" size_t))
(define free-yaml-event (foreign-lambda void "free" yaml_event_t))

(define yaml_parser_parse (foreign-lambda int
                                          "yaml_parser_parse"
                                          yaml_parser_t
                                          yaml_event_t))

(define free-yaml-parser (foreign-lambda* void ((yaml_parser_t parser))
    "yaml_parser_delete(parser);\n"
    "free(parser);\n"))

(define alloc-yaml-parser (foreign-lambda* yaml_parser_t ()
    "yaml_parser_t * parser;\n"
    "parser = malloc(sizeof(yaml_parser_t));\n"
    "yaml_parser_initialize(parser);\n"
    "C_return(parser);\n"))

(define free-yaml-parser (foreign-lambda* void ((yaml_parser_t parser))
    "yaml_parser_delete(parser);\n"
    "free(parser);\n"))

(define event.data.start_stream.encoding (foreign-lambda* int
                                                          ((yaml_event_t event))
                                                          "C_return(event->data.stream_start.encoding);"))

(define event.data.document_start.version_directive (foreign-lambda* c-pointer
                                                          ((yaml_event_t event))
                                                          "C_return(event->data.document_start.version_directive);"))

(define event.data.document_start.version_directive->major (foreign-lambda* int
                                                          ((yaml_event_t event))
                                                          "C_return(event->data.document_start.version_directive->major);"))

(define event.data.document_start.version_directive->minor (foreign-lambda* int
                                                          ((yaml_event_t event))
                                                          "C_return(event->data.document_start.version_directive->minor);"))

(define (event.data.document_start.version_directives event)
  (if (event.data.document_start.version_directive event)
      (cons (event.data.document_start.version_directive->major event)
            (event.data.document_start.version_directive->minor event))
      '()))

)
