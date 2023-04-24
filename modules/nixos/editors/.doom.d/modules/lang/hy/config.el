;; -*- no-byte-compile: t; -*-
;;; lang/hy/config.el -*- lexical-binding: t; -*-

(use-package! hy-mode
  :mode "\\.hy\\'"
  :interpreter "hy"
  :config
  (set-repl-handler! 'hy-mode #'hy-shell-start-or-switch-to-shell)
  (set-company-backend! 'hy-mode 'company-hy)
	(setq hy-shell--name-internal hy-shell--name)
	(setq hy-shell--buffer-name-internal hy-shell--buffer-name)
  (map! :map hy-mode-map
	:localleader
	"eb" #'hy-shell-eval-buffer
	"ef" #'hy-shell-eval-current-form
	"es" #'hy-shell-eval-last-sexp
	"er" #'hy-shell-eval-region
	"rd" #'hy-shell-kill-all
	"d" #'hy-insert-pdb
	"h" #'hy-describe-thing-at-point
	"rs" #'hy-reset-ns)
  )

(defun hy-reset-ns ()
	(interactive)
  (hy-shell--redirect-send-internal hy-jedhy--reset-namespace-code))
