;; -*- no-byte-compile: t; -*-
;;; snippet-helper-functions/php-mode/test/add_parameter_test.el

(describe "add parameter"
	(before-all
		(load-file "../add_parameter.el"))
	(it "should add parameter to the class"
		(with-current-buffer (get-file-buffer "./test_text.php")
			(goto-char (point-min))
			(+yas-php/add-parameter "meka")
			(expect (buffer-string) :to-match "$this->meka")
			(revert-buffer t t)
			)))
