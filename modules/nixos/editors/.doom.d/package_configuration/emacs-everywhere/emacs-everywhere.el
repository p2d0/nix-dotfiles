;;; package_configuration/emacs-everywhere/emacs-everywhere.el -*- lexical-binding: t; -*-

(setq emacs-everywhere-markdown-apps '("Brave" "Telegram" "Discord" "Element" "Fractal" "NeoChat" "Slack"))

(after! emacs-everywhere
	(setq emacs-everywhere-init-hooks
		`(emacs-everywhere-set-frame-name
    ,(cond
      ((executable-find "pandoc") #'org-mode)
      ((fboundp 'markdown-mode) #'emacs-everywhere-major-mode-org-or-markdown)
      (t #'text-mode))
    emacs-everywhere-insert-selection
    emacs-everywhere-remove-trailing-whitespace
    emacs-everywhere-init-spell-check))
  (setq emacs-everywhere-frame-parameters
    '((name . "emacs-everywhere")
       (width . 100)
       (height . 35))))

(defcustom emacs-everywhere-note-frame-parameters
  `((name . "emacs-note")
     (width . 100)
     (height . 40))
  "Parameters `make-frame' recognises to apply to the emacs-everywhere frame."
  :type 'list
  :group 'emacs-everywhere)

(defun emacs-everywhere-note ()
  "Lanuch the emacs-everywhere frame from emacsclient."
  (apply #'call-process "emacsclient" nil 0 nil
    (delq
      nil (list
	    (when (server-running-p)
	      (if server-use-tcp
		(concat "--server-file="
		  (shell-quote-argument
		    (expand-file-name server-name server-auth-dir)))
		(concat "--socket-name="
		  (shell-quote-argument
		    (expand-file-name server-name server-socket-dir)))))
	    "-c" "-F"
	    (prin1-to-string emacs-everywhere-note-frame-parameters)
	    ))))
