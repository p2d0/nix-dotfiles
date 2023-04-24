;;; editors/.doom.d/nano.el -*- lexical-binding: t; -*-

(setq default-frame-alist
	(append '((buffer-predicate . doom-buffer-frame-predicate)
		 (right-divider-width . 10)
		 (bottom-divider-width . 10)
		 (menu-bar-lines . 0)
		 (tool-bar-lines . 0)
		 (min-height . 1)
		 (height . 45)
		 (min-width . 1)
		 (width . 81)
		 (internal-border-width . 10)
		 (left-fringe . 1)
		 (right-fringe . 1)
		 (vertical-scroll-bars)) )
	)

(setq window-divider-default-right-width 10)
(setq window-divider-default-bottom-width 10)
(setq window-divider-default-places t)
(window-divider-mode 1)

;; No ugly button for checkboxes
;; (setq widget-image-enable nil)

;; Hide org markup for README
;; (setq org-hide-emphasis-markers t)


;; (setq nano-font-family-monospaced "Fira Code")
;; (setq nano-font-family-proportional nil)
;; (setq nano-font-size 10)
;; (require 'nano)
;; (require 'nano-theme-dark)
;; (require 'nano-theme-light)
;; (setq nano-theme-var "light")
;; (require 'nano-base-colors)
;; (require 'nano-faces)
;; (require 'nano-colors)
;; (require 'nano-theme)
;; (nano-refresh-theme)
;; (require 'nano-modeline)
;; (require 'nano-theme)
;; (require 'nano-theme-light)
;; (nano-refresh-theme)
