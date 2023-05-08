;;; ~/.doom.d/map.el -*- lexical-binding: t; -*-


(map! :leader "/" #'+default/search-project)
(map! :leader "ww" #'ace-window)
(map! :leader "od" #'dired-jump)

(map!
	:map dired-mode-map
	:n "q" nil)
(map!
	(:map web-mode-map
		(:i "TAB" #'+web/indent-or-yas-or-emmet-expand))
	)
(map!
	(:i "C-c C-s" #'yas-insert-snippet)
	)

;; Expand variants
(global-set-key (kbd "M-/") 'hippie-expand)
;; (global-set-key (kbd "M-/") 'evil-complete-next)

;; Put in insert mode
;; (global-set-key (kbd "C-p") 'evil-paste-after)

;; (map! :i "C-p" #'evil-paste-after)

;; (map!
;;  :leader
;;  "ap" #'list-processes)

(setq iedit-toggle-key-default nil)

;; (general-define-key
;; 	(kbd "<mouse-movement>")
;; 	(function kektest))
;; (map!
;; 	:leader
;; 	"SPC" #'consult-find)


(map!
	:leader
	"bB" #'+vertico/switch-workspace-buffer
	"bb" #'consult-buffer
	)

(map!
	:leader
	"c=" #'+format/region-or-buffer)


(map!
	:leader
	"pR" #'projectile-replace)

(after! python
  (map!
		:map python-mode-map
		:localleader
		"tt" #'python-pytest))

(map!
	"C-c C-c" #'string-inflection-lower-camelcase
	"C-c C-l" #'string-inflection-lisp
	"C-c C-a" #'string-inflection-all-cycle)

(map!
	:leader
	:desc "Switch to previous buffer" "TAB" #'evil-switch-to-windows-last-buffer
	"`" nil)

(map!
	:after treemacs
	:map treemacs-mode-map
	(:localleader
		"a" #'treemacs-run-hygen-on-directory))

(map!
	:after evil
	:map evil-normal-state-map
	"gs" #'avy-goto-char)

(map!
	:after evil-states
	:map evil-visual-state-map
	"gs" #'avy-goto-char)

(map!
	:leader
	:desc "Git time machine"
	"gt" #'git-timemachine)

(map!
	:after evil
	:v "s" #'evil-surround-region)

(defun doom/ediff-init-and-example ()
  "ediff the current `init.el' with the example in doom-emacs-dir"
  (interactive)
  (ediff-files (concat doom-user-dir "init.el")
	  (concat doom-emacs-dir "templates/init.example.el")))

(define-key! help-map
  "di"   #'doom/ediff-init-and-example)

;; (defun doom/search-lsp-folders ()
;;   (interactive)
;;   (let ((default-directory (completing-read "Select lsp directory" (lsp-session-folders (lsp-session)))))
;;     (+default/search-cwd)
;;     )
;;   )

;; (map! :leader "sl" #'doom/search-lsp-folders)
