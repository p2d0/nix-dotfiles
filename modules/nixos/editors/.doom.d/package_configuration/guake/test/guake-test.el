;; -*- no-byte-compile: t; -*-
;;; package_configuration/guake/test/guake-test.el


(describe "guake.el"
	(before-all
		(load-file "../guake.el"))
	(it "should return file path"
		(let ((path (or buffer-file-name default-directory)))
			(expect (file-name-directory path) :to-equal "/home/andrew/.doom.d/package_configuration/guake/test/"))
		)
	(it "should return command to run"))
