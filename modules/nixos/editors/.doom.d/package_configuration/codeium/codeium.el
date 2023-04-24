;;; editors/.doom.d/package_configuration/codeium/codeium.el -*- lexical-binding: t; -*-

(after! lsp
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive))
(after! emacs-lisp
  (advice-add #'elisp-completion-at-point :around #'cape-wrap-nonexclusive))
(after! fish
  (advice-add #'cape-keyword :around #'cape-wrap-nonexclusive))
(after! python
  (advice-add #'python-completion-at-point :around #'cape-wrap-nonexclusive))

(use-package! codeium
	:after company
  :hook (doom-first-buffer . codeium-init)
	:init
	(add-hook 'prog-mode-hook (lambda ()
															(setq-local completion-at-point-functions (cons (car completion-at-point-functions) (cons #'codeium-completion-at-point (cdr completion-at-point-functions))))
															)))

;; (after! codeium
;; (setq codeium-api-enabled
;;   (lambda (api)
;;     (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))

;; ;; You can overwrite all the codeium configs!
;; ;; for example, we recommend limiting the string sent to codeium for better performance
;; (defun my-codeium/document/text ()
;;   (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
;; ;; if you change the text, you should also change the cursor_offset
;; ;; warning: this is measured by UTF-8 encoded bytes
;; (defun my-codeium/document/cursor_offset ()
;;   (codeium-utf8-byte-length
;;     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;; (setq codeium/document/text 'my-codeium/document/text)
;; (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
;; (setq codeium-mode-line-enable
;;   (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;; (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;; (setq +lsp-company-backends '(:separate  company-capf company-yasnippet company-tabnine))
;; (add-hook 'lsp-mode-hook #'lsp-completion-mode)


;; (defalias 'lsp-pog (cape-capf-noninterruptible #'lsp-completion-at-point))
;; (setq-local completion-at-point-functions (list  #'lsp-pog #'codeium-completion-at-point))
;; (add-hook 'python-mode-hook
;;   (lambda ()
;;     (setq-local completion-at-point-functions (list #'lsp-completion-at-point #'codeium-completion-at-point))))
;; (add-hook 'completion-at-point-functions #'codeium-completion-at-point nil t)
;; (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;; )
