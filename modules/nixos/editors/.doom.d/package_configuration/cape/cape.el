(use-package! cape
  :after company
  :commands (cape-dabbrev
							cape-file
							cape-history
							cape-keyword
							cape-tex
							cape-sgml
							cape-rfc1345
							cape-abbrev
							cape-ispell
							cape-dict
							cape-symbol
							cape-line)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)

	(setq lsp-completion-provider :none)
  (add-hook! '(TeX-mode-hook LaTeX-mode-hook org-mode-hook)
    (lambda ()
      (add-to-list 'completion-at-point-functions #'cape-tex t))
    :depth 2)
  (add-hook! '(html-mode-hook +web-react-mode-hook typescript-tsx-mode-hook org-mode-hook markdown-mode-hook)
    (lambda ()
      (add-to-list 'completion-at-point-functions #'cape-sgml t))
    :depth 2)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :config
  ;; Enhances speed on large projects, for which many buffers may be open.
  (setq cape-dabbrev-check-other-buffers nil))
