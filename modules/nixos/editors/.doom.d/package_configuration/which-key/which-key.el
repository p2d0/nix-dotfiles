;;; editors/.doom.d/package_configuration/which-key/which-key.el -*- lexical-binding: t; -*-

(setq which-key-show-top-level 1)
(setq which-key-persistent-popup nil)

;; (defun +me/really-transient (keymap-sym)
;;   (interactive)
;;   (set-transient-map (symbol-value keymap-sym))
;;   (which-key--show-keymap
;;          (symbol-name keymap) (symbol-value keymap) nil nil t))

(defun which-key-major-mode (&rest)
  (interactive)
  (setq-local which-key-persistent-popup t)
  ;; (remove-hook 'pre-command-hook #'which-key--hide-popup t)
  (which-key-show-full-major-mode))

(map!
  (:n "S-SPC" #'which-key-show-full-major-mode)
  (:map dired-mode-map (:n "S-SPC" #'which-key-show-full-major-mode) )
  (:map woman-mode-map (:n "S-SPC" #'which-key-show-full-major-mode) )	)

;; (defun hercules---show (&optional keymap flatten transient &rest _)
;;   "Summon hercules.el showing KEYMAP.
;; Push KEYMAP onto `overriding-terminal-local-map' when TRANSIENT
;; is nil.  Otherwise use `set-transient-map'.  If FLATTEN is t,
;; show full keymap \(including sub-maps\), and prevent redrawing on
;; prefix-key press by overriding `which-key--update'."
;;   (setq hercules--popup-showing-p t
;;         which-key-persistent-popup t)
;;   (when keymap
;;     (let ((which-key-show-prefix hercules-show-prefix))
;;       (if flatten
;;           (progn
;;             (which-key--show-keymap
;;              (symbol-name keymap) (symbol-value keymap) nil t t)
;;             (advice-add #'which-key--update :override #'ignore))
;;         (which-key--show-keymap
;;          (symbol-name keymap) (symbol-value keymap) nil nil t)))
;;     (if transient
;;         (set-transient-map (symbol-value keymap)
;;                            t #'hercules--hide))))

;; (hercules-def
;;   :show-funs #'woman-mode
;;   :keymap 'woman-mode-map
;;   :transient nil)



(defun woman-which-key-setup (&rest)
  (remove-hook 'pre-command-hook 'which-key--hide-popup t))
