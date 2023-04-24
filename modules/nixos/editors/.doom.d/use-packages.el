;;; editors/.doom.d/use-packages.el -*- lexical-binding: t; -*-

(use-package! org-attach-screenshot
	:after org-mode)

;; (use-package! nano
;; 	:ensure t)

(use-package! doom-nano-modeline
  :config
  (doom-nano-modeline-mode 1)
  (global-hide-mode-line-mode 1))
;; TODO only import autoload
(use-package! benchmark-init
	:disabled
  ;; :ensure t
  ;; :config
  ;; ;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
  ;; (add-hook 'doom-first-input-hook 'benchmark-init/deactivate)
	)
;; (use-package! ssh-deploy
;; 	:after lisp-data-mode)
