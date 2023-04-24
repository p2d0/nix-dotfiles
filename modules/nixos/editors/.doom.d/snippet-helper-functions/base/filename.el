;;; snippet-helper-functions/filename.el -*- lexical-binding: t; -*-

(defun +yas/filename ()
  (file-name-sans-extension (buffer-name)))

(defun +yas/remove-from-filename (str)
  (replace-regexp-in-string str "" (+yas/filename)))
