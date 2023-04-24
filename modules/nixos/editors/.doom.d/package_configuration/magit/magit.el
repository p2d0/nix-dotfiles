;;; ~/.doom.d/magit-config.el -*- lexical-binding: t; -*-

(after! magit
  (setq magit-display-buffer-function 'display-buffer)
  (setq magit-diff-expansion-threshold 3)
  (setq magit-diff-highlight-trailing nil)
  (setq magit-diff-paint-whitespace nil)
  (setq magit-diff-highlight-hunk-body nil)
  (setq magit-section-highlight-hook '())
  (setq magit-diff-sections-hook '(magit-insert-diff))
  (setq magit-diff-highlight-hunk-region-functions '())
  (setq magit-diff-refine-hunk nil)
	(remove-hook 'server-switch-hook 'magit-commit-diff)
	(remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff))

(after! magit-section
	(defun magit-section-show (section)
		"Show the body of the current section."
		(interactive (list (magit-current-section)))
		(oset section hidden nil)
		(magit-section--maybe-wash section)
		(when-let ((beg (oref section content)))
			(when (< (- (oref section end) beg) 5000)
				(remove-overlays beg (oref section end) 'invisible t)))
		(magit-section-maybe-update-visibility-indicator section)
		(magit-section-maybe-cache-visibility section)
		(dolist (child (oref section children))
			(if (oref child hidden)
				(magit-section-hide child)
				(magit-section-show child)))))
