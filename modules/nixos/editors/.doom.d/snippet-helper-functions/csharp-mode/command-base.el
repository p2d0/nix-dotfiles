;;; snippet-helper-functions/csharp-mode/command.el -*- lexical-binding: t; -*-

(defun +yas-csharp/command-base ()
  (replace-regexp-in-string "CommandHandler" "" (+yas/filename)))
