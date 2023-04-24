;;; util.el -*- lexical-binding: t; -*-

(defun get-directory (path)
  (if (f-dir? path)
    path
    (file-name-directory
      path)))

(defun new-systemd-user-service ()
  (interactive)
  (let ((file (read-file-name "Service name" "/~/.config/systemd/user/")))
    (find-file
      (if (s-contains? ".service" file)
	file
	(s-append file ".service")))))
