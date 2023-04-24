;;; packages/todokata/todokata.el -*- lexical-binding: t; -*-

;;; todokata.el --- Test package -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Andrew Cerkin
;;
;; Author: Andrew Cerkin <https://github.com/andrew>
;; Maintainer: Andrew Cerkin <cerkin-3@yandex.ru>
;; Created: October 13, 2021
;; Modified: October 13, 2021
;; Version: 0.0.1
;; Homepage: https://github.com/andrew/todokata
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Test package
;;
;;; Code:


(defun todokata--display (&optional items)
  (with-current-buffer (get-buffer-create "*todokata*")
    ;; (read-only-mode)
    (when items
      (--each items
        (insert it "\n")) )))

(defun todokata--toggle ()
  (if (s-contains? "[x]" (thing-at-point 'line))
      (progn
        (re-search-forward "\\[x\\]")
        (delete-region (match-beginning 0) (match-end 0))
        (insert "[ ]"))
    (progn
      (re-search-forward "\\[ \\]")
      (delete-region (match-beginning 0) (match-end 0))
      (insert "[x]"))))

(defun todokata--insert ()
  (message "INSERTING")
  (insert "[x] "))

(defun todokata--mark-done ()
  (save-excursion
    (beginning-of-line)
    (prin1 (s-contains? "[" (thing-at-point 'line)))
    (if (s-contains? "[" (thing-at-point 'line))
	(todokata--toggle)
      (todokata--insert))))


(provide 'todokata)
;;; todokata.el ends here
