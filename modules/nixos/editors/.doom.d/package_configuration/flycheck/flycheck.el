;;; ~/.doom.d/flycheck-config.el -*- lexical-binding: t; -*-

(defun flycheck-toggle-list ()
  (interactive)
  (unless (get-buffer flycheck-error-list-buffer)
    (with-current-buffer (get-buffer-create flycheck-error-list-buffer)
      (flycheck-error-list-mode)))
  (if-let ((win (get-buffer-window flycheck-error-list-buffer)))
      (delete-window  win)
    (display-buffer-at-bottom (get-buffer flycheck-error-list-buffer)
                              '((window-height . 15)))))

(map! :leader
      :prefix ("e" . "Errors")
      "l" #'flycheck-toggle-list
      "n" #'flycheck-next-error
      "p" #'flycheck-previous-error)

(setq flycheck-check-syntax-automatically '(save mode-enable))
