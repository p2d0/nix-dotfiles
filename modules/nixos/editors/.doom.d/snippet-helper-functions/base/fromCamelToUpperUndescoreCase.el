;;; snippet-helper-functions/base/fromCamelToUpperUndescoreCase.el -*- lexical-binding: t; -*-

(defun +yas/to_upper_underscore_case (text)
  (upcase (s-snake-case text) ))
