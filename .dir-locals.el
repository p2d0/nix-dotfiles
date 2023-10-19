((nil . ((run-command-recipe-dir-locals-fn
          . (lambda ()
              (list
               (list
                :command-name "rebuild-default"
                :display "rebuild-default"
                :command-line "sudo nixos-rebuild switch --impure --flake '/etc/nixos/.?submodules=1#mysystem' -j6 --show-trace \
                && sudo /run/current-system/specialisation/default/activate")
               )))
         )))
