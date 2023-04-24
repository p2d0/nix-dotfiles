;;; package_configuration/markdown/markdown.el -*- lexical-binding: t; -*-

(after! markdown-mode
	(map! :map markdown-mode-map
		(:localleader
			(:n "ar" #'markdown-table-insert-row)
			(:n "ac" #'markdown-table-insert-column)
			(:n "al" #'markdown-table-align))
		)
	)
