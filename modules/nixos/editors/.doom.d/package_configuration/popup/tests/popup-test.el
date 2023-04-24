;; -*- no-byte-compile: t; -*-
;;; package_configuration/popup/tests/popup-test.el -*- lexical-binding: t; -*-


(describe "popup"
	(describe "popup--popup-buffer? "
		(it "should display that buffer is a popup if it is"
			(let* ((buf-name "*TestBuffer*")
							(buf (get-buffer-create buf-name)))
				(with-current-buffer buf
					(+popup-buffer-mode)
					(expect (+popup--popup-buffer? (current-buffer)) :to-be-truthy))
				(expect  (cl-remove-if-not #'+popup--popup-buffer? (buffer-list)) :to-contain buf))))
	(describe "popup-find-rules"
		(it "should find rules"
			)
		))
