((nil . ((run-command-recipe-dir-locals-fn
          . (lambda ()
              (list
               (list
                :command-name "test"
                :display "build nix file"
                :cache-variables `(:name ,(file-relative-name (buffer-file-name) ))
                :command-line (lambda ()
                                "test"
                                (format "NIXPKGS_ALLOW_BROKEN=1 nix-build --impure %s" (plist-get cache-variables :name)))
                ;; (lambda ()
                ;;   (format "nix-build %s" (file-relative-name (buffer-file-name) ))
                ;;   "pwd"
                ;;   )
                )
               )))
         )))
