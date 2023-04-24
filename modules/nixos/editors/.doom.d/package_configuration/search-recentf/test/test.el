;; -*- no-byte-compile: t; -*-
;;; package_configuration/search-recentf/test/test.el

(require 'test-simple)
(test-simple-start)

(assert-t (load-file "../search-recentf.el")
  "Cant load search-recentf");
(note "test push to recentf")

(let ((folder "/home/andrew/.doom.d/package_configuration/search-recentf/"))
  (+config/push-to-recentf folder)
  (assert-equal folder (first config-recentf)))


(end-tests)
