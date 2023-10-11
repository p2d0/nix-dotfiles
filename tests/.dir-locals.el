((nil . ((run-command-recipe-dir-locals-fn
          . (lambda ()
              (list
               (list
                :command-name "Run ALL tests"
                :display "Run ALL tests"
                :command-line "nix-instantiate --eval --strict . -A tests."
                )
               (list
                :command-name "Run current file tests"
                :display "Run Current file tests"
                :command-line (lambda ()
                                (format "nix-instantiate --eval --strict . -A tests.%s" (file-name-sans-extension (file-relative-name (buffer-file-name)))))
                )
               )))
         )))
