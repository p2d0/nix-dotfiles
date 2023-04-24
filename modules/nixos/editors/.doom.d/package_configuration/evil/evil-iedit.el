;;; ~/.doom.d/evil-iedit-config.el -*- lexical-binding: t; -*-
;; (require 'evil-iedit-state)
;; (defalias 'iedit-cleanup 'iedit-lib-cleanup)

;; (map! (:leader
;;        "se" #'evil-iedit-state/iedit-mode))

(defun evil-multiedit-match-and-next-and-state (&rest args)
  (interactive)
  (evil-multiedit-match-and-next)
  (evil-multiedit-state))

(map!
  :v "R" #'evil-multiedit-toggle-or-restrict-region
  (:leader
    "se" #'evil-multiedit-match-and-next-and-state
    "sa" #'evil-multiedit-match-all
    )
  (:map evil-multiedit-mode-map
    "C-n" #'evil-multiedit-next
    "C-p" #'evil-multiedit-prev
    "C-j" #'evil-multiedit-match-and-next
    (:n "L" #'iedit-restrict-current-line )
    (:n "K" #'evil-multiedit-match-and-prev)
    (:n "J" #'evil-multiedit-match-and-next)
    "C-k" #'evil-multiedit-match-and-prev
    (:n "F" #'iedit-restrict-function)
    (:n "S" #'evil-multiedit--change-line)
    )
  )
