;;; package_configuration/org-pomodoro/org-pomodoro.el -*- lexical-binding: t; -*-

(use-package! org-pomodoro
	:after org-mode
	:init
	(map!
		:after org
		:map 'org-mode-map
		:localleader "cp" #'org-pomodoro)
	:config
	(setq org-pomodoro-ticking-sound-p t)
	)

