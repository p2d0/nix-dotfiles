;;; package_configuration/debug/debug.el -*- lexical-binding: t; -*-


(defhydra debug-hydra (:color pink :hint nil)
  "
^Stepping^              ^Debug^                     ^Eval
^^^^^^^^---------------------------------------------------------------------
_d_: Next/Step through  _s_: Debug show locals      _e_: Eval expression
_c_: Continue                                     _R_: Record expression

"
  ("d" (progn
	 (debugger-step-through)
	 (debug-hydra/body)
	 ))
  ("c" debugger-continue)
  ;; ("j" debugger-jump)
  ("R" debugger-record-expression)
  ("s" backtrace-toggle-locals)
  ("e" debugger-eval-expression)
  ("R" debugger-record-expression)
  ("q" debugger-quit "quit" :color blue))


(defadvice! +debugger-reenable-hydra ()
  "Start debugger-hydra after reenable"
  :after #'debugger-reenable
  (debug-hydra/body))

;; Is this the best way to do it?
(defadvice! +debugger-start-hydra (buf &rest args)
  "Start debugger-hydra on debug"
  :after #'pop-to-buffer
  (when (derived-mode-p #'debugger-mode)
    (debug-hydra/body)))

(after! debugger
  (add-hook 'debugger-mode-hook #'debug-hydra/body))

(after! emacs-lisp-mode
  (map! :map emacs-lisp-mode-map
	:localleader
	"df" #'debug-on-entry
	"dF" #'cancel-debug-on-entry))
