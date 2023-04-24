;;; editors/.doom.d/package_configuration/dired/dired.el -*- lexical-binding: t; -*-

(defun dired--do-shell (command-string)
  (interactive)
  (dired-do-shell-command
   command-string current-prefix-arg
   (dired-get-marked-files t current-prefix-arg)))

(defun dired-jpg-down-to ()
	(interactive)
	(dired--do-shell (s-concat "jpegoptim -S " (read-string "Size in Kb default 768: " nil nil "768") " ?" )))
