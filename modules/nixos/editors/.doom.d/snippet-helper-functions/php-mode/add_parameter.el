;; -*- no-byte-compile: t; -*-
;;; snippet-helper-functions/php-mode/test/add_parameter.el

(defun +yas-php/add-parameter (text)
	(save-excursion
		(search-forward "setUp")
		(forward-line 2)
		(insert text ";\n"))
	)
