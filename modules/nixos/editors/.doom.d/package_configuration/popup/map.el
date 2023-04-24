;;; package_configuration/popup/map.el -*- lexical-binding: t; -*-


(map!
	(:leader
		"`" #'+popup/toggle
		"bp" #'+popup/buffers
		"~" #'+popup/restore)
		"C-`" #'+popup/buffer

	)
