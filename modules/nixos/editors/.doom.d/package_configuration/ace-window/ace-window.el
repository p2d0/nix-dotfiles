;;; package_configuration/ace-window/ace-window.el -*- lexical-binding: t; -*-

(after! ace-window
	(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
	(custom-set-faces!
		'(aw-leading-char-face
			 :foreground "white" :background "red"
			 :weight bold :height 2.5 :box (:line-width 10 :color "red")))
	(setq aw-ignore-on nil)
	(setq aw-scope 'global)
	)
