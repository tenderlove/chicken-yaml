# A YAML parser for Chicken

This wraps libyaml and exposes it to Chicken.  It exposes a few functions.
These are the main 3:

* yaml-load
* yaml-parse
* yaml-dump

The first loads a YAML document and translates it to a scheme data structure,
the second calls callbacks on each YAML event, and the third dumps a scheme
structure to yaml.

## Loading Whole Documents

### Strings, Symbols, Null

```scheme
#;1> (import yaml)
#;2> (yaml-load "--- foo")
"foo"
#;3> (yaml-load "--- :bar")
bar
#;4> (yaml-load "--- ")
#<sql-null-type>
```

### Lists

```scheme
#;1> (import yaml)
#;2> (yaml-load "--- ['foo', ['bar']]")
("foo" ("bar"))
```

### Hashes

```scheme
#;1> (import yaml)
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

## Dumping structures

### Dumping to a string

You can dump whole structures to a string like this:

```scheme
#;1> (import yaml)
#;2> (print (yaml-dump (list "foo" "bar" (list (cons "baz" "omg")))))
---
- foo
- bar
- baz: omg

#;3>
```

### Dumping to a port

Or you can dump structures to a port:

```scheme
#;1> (import yaml)
#;2> (yaml-dump (list "foo" "bar" (list (cons "baz" "omg"))) (current-output-port))
---
- foo
- bar
- baz: omg
```
