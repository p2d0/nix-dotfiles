;;; ~/.doom.d/package_configuration/fennel-mode.el -*- lexical-binding: t; -*-

(after! fennel
	(map!
		:map fennel-mode-map
		:localleader

		(:prefix ("g" . "Go to")

			:desc "Definition"
			"d" #'fennel-find-definition

			:desc "Definition new window"
			"D" #'fennel-find-definition-pop)

		:desc "Open fennel repl"
		"r" #'fennel-start-nrepl

		:desc "Eval buffer"
		"eb" #'fennel-eval-buffer

		:desc "View compilation"
		"c" #'fennel-view-compilation

		:desc "Reload"
		"R" #'fennel-reload)

	)
