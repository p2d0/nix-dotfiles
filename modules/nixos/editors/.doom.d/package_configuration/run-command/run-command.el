;;; editors/.doom.d/package_configuration/run-command/run-command.el -*- lexical-binding: t; -*-
;; Examples
;; ls[lulz]
;; https://github.com/bard/emacs-run-command/tree/master/examples
;; Replacing path
;; :command-line "ls -al | sed 's/bashrc//'" NOTE Doesnt work
;; (setq directory-abbrev-alist '(("/var/www/html" . "..."))) NOTE Works
;; DOCKER:
;; :command-line
;; (lambda ()
;;   (setq directory-abbrev-alist '(("/var/www/html" . "...")))
;;   "...")
;;  CACHING:


(defvar run-command--last nil)
(defvar run-command-recipe-dir-locals-fn nil)


(defun run-command-recipe-dir-locals ()
  (when run-command-recipe-dir-locals-fn
    (funcall run-command-recipe-dir-locals-fn)))

(load! "run-command-recipe-package-json.el")

; Run a script from the project's package.json file. Supports both npm and yarn.
; Run package.json scripts END

(defun run-command-rerun ()
  (interactive)
  (when run-command--last
    (run-command--run run-command--last)))



(after! run-command
  (advice-add #'run-command--run :after (lambda (command-spec) (setq run-command--last command-spec)))
  (set-popup-rule! "^.+\\[.+\\]$"
    :size 16
    :quit t)
  (setq run-command-recipes '(run-command-recipe-dir-locals run-command-recipe-package-json)))

(map!
  :leader
  "cc" #'run-command
  "cC" #'run-command-rerun
  )

