;;; package_configuration/vertico.el -*- lexical-binding: t; -*-

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
  (lambda (&rest args)
    (apply (if vertico-mode
	     #'consult-completion-in-region
	     #'completion--in-region)
      args)))
