;;; ~/.doom.d/hydra-config.el -*- lexical-binding: t; -*-
(after! evil
	(defhydra hydra-put (:color red :columns 2)
		"Paste"
		("C-j" evil-paste-pop-next "Next")
		("C-k" evil-paste-pop "Prev"))
	(defadvice! +evil-paste-after-hydra (&rest _)
		"Start debugger-hydra after reenable"
		:after #'evil-paste-after
		(hydra-put/body))
	(defadvice! +evil-paste-before-hydra (&rest _)
		"Start debugger-hydra after reenable"
		:after #'evil-paste-before
		(hydra-put/body))
	;; (general-def
	;; 	:prefix-map 'custom-paste-map
	;; 	"C-j" #'evil-paste-pop-next
	;; 	"C-k" #'evil-paste-pop)
	;; (hercules-def
	;; 	:show-funs '(evil-paste-after evil-paste-before)
	;; 	:keymap 'custom-paste-map
	;; 	:transient t)
	)
