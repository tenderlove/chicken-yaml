# A YAML parser for Chicken

```scheme
; loading /Users/aaron/.csirc ...
#;1> (use yaml)
; loading ./yaml.import.so ...
; loading ./yaml.so ...
#;2> (yaml-load "--- ['foo', ['bar']]")
("foo" ("bar"))
```
