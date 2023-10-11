((nil . ((run-command-recipe-dir-locals-fn
          . (lambda ()
              (list
               (list
                :command-name "build nix file"
                :display "build nix file"
                :command-line (lambda ()
                                (format "nix-build %s" (file-relative-name (buffer-file-name) )))
                )
               )))
         )))
