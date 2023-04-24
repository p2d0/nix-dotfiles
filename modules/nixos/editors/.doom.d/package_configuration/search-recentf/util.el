;;; package_configuration/search-recentf/util.el -*- lexical-binding: t; -*-

;; (defun dump-vars-to-file (var filename)
;;   "simplistic dumping of variables in VARLIST to a file FILENAME"
;;   (save-excursion
;;     (let ((buf (find-file-noselect filename)))
;;       (set-buffer buf)
;;       (erase-buffer)
;;       (dump var buf)
;;       (save-buffer)
;;       (kill-buffer))))

;; (defun dump (var buffer)
;;   "insert into buffer the setq statement to recreate the variables in VARLIST"
;;   (print (list 'setq var (list 'quote (symbol-value var)))
;;     buffer))
