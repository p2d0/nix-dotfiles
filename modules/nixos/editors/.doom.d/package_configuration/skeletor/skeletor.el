;;; editors/.doom.d/package_configuration/skeletor/skeletor.el -*- lexical-binding: t; -*-
;; Defining overrides https://github.com/chrisbarrett/skeletor.el/pull/48/commits/f8af555a6d796c55b6faec854a33a6b08426ff21
;; For example you can now create a file with the following template name:
;; foo__(alist-get "__PROJECT-NAME__" subs nil nil #'string-equal)__bar

(use-package! skeletor
	:defer t
	:config
	(defun skeletor-sub (variable subs)
		(alist-get variable subs nil nil #'string-equal))
	(defun skeletor--process-macro-args (args)
    "Check ARGS are well-formed, then process them into an alist."
    (-let* (((name . keys) args)
						 (arg-alist (skeletor--plist-to-alist keys)))
      (skeletor--validate-macro-arguments name arg-alist)
      (let-alist arg-alist
				(list (cons 'constructor-fname (intern (format "skeletor--create-%s" name)))
					(cons 'title (or .title (s-join " " (-map 's-capitalize (s-split-words name)))))
					(cons 'name name)
					(cons 'project-name .project-name)
					(cons 'use-git? (not .no-git?))
					(cons 'initialise-fn .initialise)
					(cons 'before-git (or .before-git 'ignore))
					(cons 'after-creation (or .after-creation 'ignore))
					(cons 'create-license? (not .no-license?))
					(cons 'license-file-name (or .license-file-name "COPYING"))
					(cons 'default-license-var (intern (format "%s-default-license" name)))
					(cons 'substitutions (eval .substitutions))
					(cons 'required-executables (eval .requires-executables))))))
	(defun skeletor--ctor-runtime-spec (spec)
    "Concatenate the given macro SPEC with values evaluated at runtime."
    (let ((project-name (or (alist-get 'project-name spec)
													(skeletor--read-project-name) )))
      (let-alist spec
				(-concat (list
									 (cons 'project-name project-name)
									 (cons 'project-dir skeletor-project-directory)
									 (cons 'dest (f-join skeletor-project-directory project-name))
									 (cons 'skeleton (skeletor--get-named-skeleton .name))
									 (cons 'license-file
										 (when .create-license?
											 (skeletor--read-license "License: " .license-file-name)))
									 (cons 'repls (-map 'skeletor--eval-substitution
																	(-concat
																		skeletor-global-substitutions
																		(list (cons "__PROJECT-NAME__" project-name)
																			(cons "__LICENSE-FILE-NAME__" .license-file-name))
																		.substitutions))))
					spec))))

	(setq skeletor-python-bin-search-path '("/usr/bin" "~/.nix-profile/bin" "/run/current-system/sw/bin"))
  (add-to-list 'skeletor--legal-keys 'project-name)

	(require 'string-inflection)
  (setq skeletor-completing-read-function #'completing-read-default)
  (setq skeletor-user-directory "~/.doom.d/skeletor-templates")

	(eval
		'(skeletor-define-template "python-mini-project"
			 :title "Python mini project"
			 :no-license? t))
	(eval '(skeletor-define-template "python-script-with-tests"
					 :title "Python script with tests"
					 :no-license? t))
  (eval '(skeletor-define-template "prestashop_upgrade"
					 :title "prestashop upgrade version template"
					 :project-name "upgrade"
					 :no-license? t
					 :no-git? t
					 :substitutions '(("__VERSION__" . (lambda () (read-string "Version in format 1.0.0: "))))))
  (eval '(skeletor-define-template "prestashop_controller"
					 :title "Prestashop controller"
					 :project-name "controllers"
					 :substitutions
					 '(("__CONTROLLER-TYPE__" .
							 (lambda ()
								 (setq controller-type (completing-read "Controller type: " '("admin" "front")))))
							("__CONTROLLER-NAME__" .
								(lambda ()
									(let* ((controller-name (read-string "Controller name: ")))
										(if (string-equal controller-type "admin")
											(concat "Admin"
												(string-inflection-pascal-case-function controller-name)
												"Controller")
											(concat
												(string-inflection-pascal-case-function controller-name)
												"ModuleFrontController")))
									))
							("__CONTROLLER-EXTEND__" .
								(lambda ()
									(if (string-equal controller-type "admin")
										"ModuleAdminController"
										"ModuleFrontController"))))
					 :no-license? t
					 :no-git? t))

	)

(map! :leader "pc" #'skeletor-create-project-at)

;; TODO mini templates without specifiying folder name
;; TODO hygen

;; (defun skeletor-create-mini-at (dir skeleton)
;;   "Interactively create a new project with Skeletor.

;; DIR is destination directory, which must exist.

;; SKELETON is a SkeletorProjectType."
;;   (interactive (list (read-directory-name "Create at: " nil nil t)
;;                      (skeletor--read-project-type)))
;;   ;; Dynamically rebind the project directory.
;;   (let ((skeletor-project-directory dir))
;;     (skeletor-create-project skeleton)))

