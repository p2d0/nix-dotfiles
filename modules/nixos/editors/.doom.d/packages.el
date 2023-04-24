;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.
;; (package! evil-iedit-state)
(package! emacsql-sqlite3)
(package! dired-omit-mode :disable t)
;; (package! myron-themes :recipe
;; 	(:host github :repo "neeasade/myron-themes" :files ("*.el" "themes/*.el")))
;; (package! nano-emacs
;;   :recipe (:host github :repo "rougier/nano-emacs"))
(package! doom-nano-modeline
  :recipe (:host github
  :repo "ronisbr/doom-nano-modeline"))
(package! groovy-mode)
;; (package! hercules)
(package! evil-snipe :disable t)
;; (package! mail-parse :disable t)
(package! org-pomodoro)
;; (package! fd-dired :disable t)
(package! evil-motion-trainer :recipe (:host github :repo "martinbaillie/evil-motion-trainer" :branch "master"))
(package! with-simulated-input)
;; (package! dap-mode)
(package! string-inflection)
(package! org-attach-screenshot)
;; (package! smartparens :disable t)
(package! company-fish :recipe (:host github :repo "Patriot720/company-fish" :branch "master"))
(package! font-lock-ext)
;; (package! todokata
;;   :recipe (:local-repo "packages/todokata"
;;            :build (:not compile)))
;; (package! vlf)
(package! reverse-im :disable t)
(package! skeletor)
;; (package! consult-taskrunner
;;   :recipe (:local-repo "packages/consult-taskrunner"
;;  	    :build (:not compile)))
(package! run-command)
;; (package! company-tabnine :recipe (:host github :repo "TommyX12/company-tabnine"))
(package! logview)
;; (package! eaf)

;; (package! openai
;;   :recipe (:host github :repo "emacs-openai/openai" :files ("dist" "*.el")))
;; (package! gptel
;;   :recipe (:host github :repo "karthink/gptel" :files ("dist" "*.el")))

;; (package! chatgpt
;;   :recipe (:host github :repo "emacs-openai/chatgpt" :files ("dist" "*.el")))

;; (package! chatgpt
;;   :recipe (:local-repo "packages/ChatGPT.el"
;;  	    :build (:not compile)))

;; (package! chatgpt
;;   :recipe (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el")))
;; (package! gimpmode)
;; (package! copilot
;;   :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(unpin! org-roam)
(unpin! ox-hugo)
(unpin! nix-mode)

(package! cape)

(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! ssh-deploy :recipe (:branch "master"))
(package! dts-mode)
(package! benchmark-init)
(package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))

;; EAF
;; (package! ctable :recipe (:host github :repo "kiwanami/emacs-ctable"))
;; (package! deferred :recipe (:host github :repo "kiwanami/emacs-deferred"))
;; (package! epc :recipe (:host github :repo "kiwanami/emacs-epc")))
;; (package! eaf :recipe (:host github
;;                              :repo "manateelazycat/emacs-application-framework"
;;                              :files ("*.el" "*.py" "app" "core")
;;                              :build (:not compile))

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
