;;; package_configuration/compile.el -*- lexical-binding: t; -*-

(after! compile
  (map!
    :map compilation-shell-minor-mode-map
    :n "C-k" #'compilation-previous-error
    :n "C-j" #'compilation-next-error
    ))

(after! js2-mode
  (defconst jest-error-match "at.+?(\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)")

  (eval-after-load 'compile
    (lambda ()
      (dolist
	(regexp
	  `((jest-error
	      ,jest-error-match
	      1 2 3
	      )))
	(add-to-list 'compilation-error-regexp-alist-alist regexp)
	(add-to-list 'compilation-error-regexp-alist (car regexp))))))



(after! php-mode
  (defconst php-error-match "\\(\/.+?\\):\\([0-9]+\\)")
  (add-to-list 'compilation-error-regexp-alist 'php-compile)
  (add-to-list 'compilation-error-regexp-alist-alist
    `(php-compile ,php-error-match 1 2 nil nil)
    )
  )
