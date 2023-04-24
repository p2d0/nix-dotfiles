;;; snippet-helper-functions/csharp-mode/entity-type-configuration-base.el -*- lexical-binding: t; -*-



(defun +yas-csharp/entity-type-configuration-base ()
  (replace-regexp-in-string "TypeConfiguration" "" (+yas/filename)))
