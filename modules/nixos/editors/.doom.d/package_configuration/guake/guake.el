;;; guake.el -*- lexical-binding: t; -*-


(defun guake-open (path)
	(start-process "" nil "guake" "--show" "-n" path ) )


(defun treemacs-visit-node-in-guake ()
  "Open file in guake"
  (interactive)
  ;; code adapted from ranger.el
  (-if-let (path (treemacs--prop-at-point :path))
    (let ((process-connection-type nil)
	   (path (if (f-dir? path)
		   path
		   (file-name-directory
		     path) )))
      (message path)
      (guake-open path)
      )
    (_ (treemacs-pulse-on-failure "Don't know how to open files on %s."
	 (propertize (symbol-name system-type) 'face 'font-lock-string-face)))
    (treemacs-pulse-on-failure "Nothing to open here.")))

(defun guake-open-current-file-dir ()
  (interactive)
  (let ((path (or buffer-file-name default-directory)))
    (guake-open (file-name-directory path) )))

(defun guake-open-current-project-dir ()
  (interactive)
  (guake-open (doom-project-root)))

(map!
  :leader
  "fg" #'guake-open-current-file-dir
  "pg" #'guake-open-current-project-dir)

(map!
  :map treemacs-mode-map
  "og" #'treemacs-visit-node-in-guake)
