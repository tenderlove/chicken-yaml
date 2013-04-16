;;;; yaml.scm
;;;; Bindings to libyaml

(module yaml
  (make-yaml-parser)

(import scheme chicken foreign)

#>
#include <yaml.h>
<#

(define make-yaml-parser 42)
)
