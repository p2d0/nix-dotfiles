;;; move-buffer-to-window.el -*- lexical-binding: t; -*-

;; Note that this duplicates code from select-window-by-number, ideally should
;; upstream this function into windows.el
(defun spacemacs/get-window-by-number (i)
  (let ((windows winum--window-vector)
        window)
    (if (and (>= i 0) (< i 10)
             (setq window (aref windows i)))
        window
      (error "No window numbered %s" i))))

(defun spacemacs/move-buffer-to-window (windownum follow-focus-p)
  "Moves a buffer to a window, using the spacemacs numbering. follow-focus-p
   controls whether focus moves to new window (with buffer), or stays on
   current"
  (interactive)
  (let ((b (current-buffer))
        (w1 (selected-window))
        (w2 (spacemacs/get-window-by-number windownum)))
    (unless (eq w1 w2)
      (set-window-buffer w2 b)
      (switch-to-prev-buffer)
      (unrecord-window-buffer w1 b)))
  (when follow-focus-p (select-window (spacemacs/get-window-by-number windownum))))

(defun spacemacs/swap-buffers-to-window (windownum follow-focus-p)
  "Swaps visible buffers between active window and selected window.
   follow-focus-p controls whether focus moves to new window (with buffer), or
   stays on current"
  (interactive)
  (let* ((b1 (current-buffer))
         (w1 (selected-window))
         (w2 (spacemacs/get-window-by-number windownum))
         (b2 (window-buffer w2)))
    (unless (eq w1 w2)
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (unrecord-window-buffer w1 b1)
      (unrecord-window-buffer w2 b2)))
  (when follow-focus-p (select-window-by-number windownum)))

(dotimes (i 9)
  (let ((n (+ i 1)))
    (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
              ,(format "Move buffer to the window with number %i." n)
              (interactive "P")
              (if arg
                  (spacemacs/swap-buffers-to-window ,n t)
                (spacemacs/move-buffer-to-window ,n t))))))
