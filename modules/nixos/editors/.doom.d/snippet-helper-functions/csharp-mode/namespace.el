(defun +yas-csharp/namespace ()
  (s-replace "/" "."
    (string-remove-suffix "/"
      (file-relative-name default-directory
	(projectile-project-root)))))
