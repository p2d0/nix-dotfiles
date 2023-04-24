;;; package_configuration/nautilus/nautilus.el -*- lexical-binding: t; -*-
(defun nautilus-open (path)
  (start-process "" nil "nautilus" path) )

(defun nautilus-open-current-file-dir ()
  (interactive)
  (let ((path (or buffer-file-name default-directory)))
    (nautilus-open (file-name-directory path) )))
