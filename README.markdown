# A YAML parser for Chicken

This wraps libyaml and exposes it to Chicken.  It exposes two functions:

* yaml-load
* yaml-parse

The former loads a YAML document and translates it to a scheme data structure,
and the latter calls callbacks on each YAML event.

## Loading Whole Documents

### Strings, Symbols, Null

```scheme
#;1> (use yaml)
#;2> (yaml-load "--- foo")
"foo"
#;3> (yaml-load "--- :bar")
bar
#;4> (yaml-load "--- ")
#<sql-null-type>
```

### Lists

```scheme
#;1> (use yaml)
#;2> (yaml-load "--- ['foo', ['bar']]")
("foo" ("bar"))
```

### Hashes

```scheme
#;1> (use yaml)
#;2> (yaml-load "--- {foo: bar}")
(("foo" . "bar"))
#;3> (yaml-load "--- {foo: bar, bar: baz}")
(("foo" . "bar") ("bar" . "baz"))
#;4>
```

## Event based parsing

The `yaml-parse` function takes many lambdas as parameters along with a seed
value.  The lambdas will be called on each yaml event.  The parameters to the
lambdas are values for that event along with the seed value.  The return value
of the lambda will be passed to the next lambda as the seed.

You can use the seed to gather a list of events like so:

```scheme
(define (yaml-exp yaml)
  (yaml-parse yaml
              (lambda (enc seed) (cons (list 'stream-start enc) seed))
              (lambda (seed) (cons (list 'stream-end) seed))
              (lambda (version tags seed) (cons (list 'document-start version tags) seed))
              (lambda (implicit? seed) (cons (list 'document-end implicit?) seed))
              (lambda (alias seed) (cons (list 'alias alias) seed))
              (lambda (value anchor tag plain quoted style seed)
                (cons (list 'scalar value anchor tag plain quoted style) seed))
              (lambda (anchor tag implicit style seed)
                (cons (list 'sequence-start
                                         anchor
                                         tag
                                         implicit
                                         style) seed))
              (lambda (seed) (cons '(sequence-end) seed))
              (lambda (anchor tag implicit style seed)
                (cons (list 'mapping-start
                                         anchor
                                         tag
                                         implicit
                                         style) seed))
              (lambda (seed) (cons '(mapping-end) seed))
              '()))

(print (yaml-exp "--- { }"))
```
