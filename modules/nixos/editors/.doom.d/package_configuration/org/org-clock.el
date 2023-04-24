;;; editors/.doom.d/package_configuration/org/org-clock.el -*- lexical-binding: t; -*-
(after! org-mode
	(require 'org-pomodoro)
	(defun my/org-clock-query-out ()
		"Ask the user before clocking out.
This is a useful function for adding to `kill-emacs-query-functions'."
		(if (and
					(featurep 'org-clock)
					(funcall 'org-clocking-p)
					(y-or-n-p "You are currently clocking time, clock out? "))
			(org-clock-out)
			t)) ;; only fails on keyboard quit or error

	;; timeclock.el puts this on the wrong hook!
	(add-hook 'kill-emacs-query-functions 'my/org-clock-query-out)
	;; (add-hook 'kill-emacs-query-functions '+workspace/close-window-or-workspace)

	(defun my/quit-emacs ()
		(interactive)
		(run-hooks 'kill-emacs-query-functions)
		(save-buffers-kill-terminal))
	(map!
		:leader "qq" #'my/quit-emacs)

	(defvar org-clock-timer nil)
	(add-hook 'org-clock-in-hook
		(lambda () (setq org-clock-timer (run-with-timer t 1 'org-clock-tick))))

	(defun org-clock-tick ()
		"A callback that is invoked by the running timer each second.
It checks whether we reached the duration of the current phase, when 't it
invokes the handlers for finishing."
		(when (and (not (org-clocking-p)) org-clock-timer)
			(cancel-timer org-clock-timer))

		(when (org-clocking-p)
			;; The first element of a time value is the high-order part of the seconds
			;; value. This is less than 0 if org-pomodoro-end-time is in the past of
			;; the current-time.
			(org-pomodoro-maybe-play-sound :tick))))
