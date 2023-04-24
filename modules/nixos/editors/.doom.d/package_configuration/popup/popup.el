;;; package_configuration/popup.el -*- lexical-binding: t; -*-

(set-popup-rule! "^\\*WoMan" :ignore t)
;; (set-popup-rule! "^\\*Help" :ignore t)
(set-popup-rule! "^\\*cider-test-report" :ignore t)

(defun +popup--popup-buffer? (buffer)
  (string-match-p  "\\*" (buffer-name buffer)))

(defun +popup/buffers ()
  (interactive)
  (display-buffer
   (completing-read  "Popup buffers: " (cl-mapcar (lambda (b) (s-trim (buffer-name b) )) (cl-remove-if-not #'+popup--popup-buffer? (buffer-list)) )) ))
