;;; package_configuration/yadm.el -*- lexical-binding: t; -*-
(after! tramp
	;; (require 'tramp)
	(add-to-list 'tramp-methods
		'("yadm"
		   (tramp-login-program "yadm")
		   (tramp-login-args (("enter")))
		   (tramp-login-env (("SHELL") ("/bin/sh")))
		   (tramp-remote-shell "/bin/sh")
		   (tramp-remote-shell-args ("-c")))))

(defun yadm-status ()
	(interactive)
	(magit-status "/yadm::"))

(defun yadm-stage ()
	(interactive)
	(let ((file
					(let ((default-directory "~/"))
						(read-file-name "Stage file: "))))
		(if (equal (expand-file-name file)
					(expand-file-name "~/.yadm/"))
			(user-error "Can't stage yadm dir itself.")
			(magit-with-toplevel
				(magit-stage-1 nil (list file))))))

(defun yadm-add ()
	(interactive)
	(let ((file-path buffer-file-name))
		(when (s-contains? "/home/" file-path)
			(shell-command (concat "yadm add " file-path)) )))

(map!
	:leader
	"gd" #'yadm-status
	"gs" #'yadm-add
	)
