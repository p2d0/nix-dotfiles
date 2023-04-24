;;; package_configuration/evil-motion-trainer.el -*- lexical-binding: t; -*-

(after! evil
	(evil-motion-trainer-mode)
	(global-evil-motion-trainer-mode 1)
	(setq evil-motion-trainer-threshold 10)
	(after! evil-motion-trainer-mode
		(global-evil-motion-trainer-mode 1)
		(setq evil-motion-trainer-threshold 10)))
