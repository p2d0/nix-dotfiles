(require 'test-simple)
(test-simple-start)

(assert-t (load-file "../fromCamelToUpperUndescoreCase.el")
  "Cant load fromCamelToUpperUndescoreCase");

(note "test 1")
(assert-equal "SOME_CAMEL_CASE_STRING" (+yas/to_upper_underscore_case "someCamelCaseString"))



(end-tests)
