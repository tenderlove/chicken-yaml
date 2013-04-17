;;;; yaml.scm
;;;; Bindings to libyaml

(module yaml
  (yaml-parse)

(import scheme chicken foreign)

#>
#include <yaml.h>
<#

(define-record-type yaml-parser
  (yaml-wrap-parser parser stream-start)
  yaml-parser?
  (parser yaml-unwrap-parser)
  (stream-start yaml-stream-start))

(define-foreign-type yaml_parser (c-pointer "yaml_parser_t"))

(define (add-tag pair tags) (append tags (list pair)))

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
              add-tag))

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
                                          (scheme-object add_tag))
    "yaml_parser_t * parser;
    int done = 0;
    int state = 0;
    yaml_event_t event;
    parser = malloc(sizeof(yaml_parser_t));
    yaml_parser_initialize(parser);
    yaml_parser_set_input_string(
      parser,
      (const unsigned char *)(yaml),
      (size_t)strlen(yaml)
    );
    while(!done) {
      if(!yaml_parser_parse(parser, &event)) {
        // FIXME
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
                  C_word *a = C_alloc(C_SIZEOF_STRING(strlen(start->handle)));
                  handle = C_string2(&a, start->handle);
                }

                if (start->prefix) {
                  C_word *a = C_alloc(C_SIZEOF_STRING(strlen(start->prefix)));
                  prefix = C_string2(&a, start->prefix);
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
              a = C_alloc(C_SIZEOF_STRING(strlen(event.data.alias.anchor)));
              alias_str = C_string2(&a, event.data.alias.anchor);
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
            val = C_string(&a, event.data.scalar.length, event.data.scalar.value);

            if (event.data.scalar.anchor) {
              C_word *a = C_alloc(C_SIZEOF_STRING(strlen(event.data.scalar.anchor)));
              anchor = C_string2(&a, event.data.scalar.anchor);
            }

            if (event.data.scalar.tag) {
              C_word *a = C_alloc(C_SIZEOF_STRING(strlen(event.data.scalar.tag)));
              tag = C_string2(&a, event.data.scalar.tag);
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
              C_word *a = C_alloc(C_SIZEOF_STRING(strlen(event.data.sequence_start.anchor)));
              anchor = C_string2(&a, event.data.sequence_start.anchor);
            }

            if (event.data.sequence_start.tag) {
              C_word *a = C_alloc(C_SIZEOF_STRING(strlen(event.data.sequence_start.tag)));
              anchor = C_string2(&a, event.data.sequence_start.tag);
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
              C_word *a = C_alloc(C_SIZEOF_STRING(strlen(event.data.mapping_start.anchor)));
              anchor = C_string2(&a, event.data.mapping_start.anchor);
            }

            if (event.data.mapping_start.tag) {
              C_word *a = C_alloc(C_SIZEOF_STRING(strlen(event.data.mapping_start.tag)));
              anchor = C_string2(&a, event.data.mapping_start.tag);
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
    C_return(seed);
    "
))

(define alloc-yaml-parser (foreign-lambda* yaml_parser ()
    "yaml_parser_t * parser;\n"
    "parser = malloc(sizeof(yaml_parser_t));\n"
    "yaml_parser_initialize(parser);\n"
    "C_return(parser);\n"))

(define free-yaml-parser (foreign-lambda* void ((yaml_parser parser))
    "yaml_parser_delete(parser);\n"
    "free(parser);\n"))
)
