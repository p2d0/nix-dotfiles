;;; package_configuration/nix/nix.el -*- lexical-binding: t; -*-

(after! nix-mode
  (setq lsp-nix-server-path "nil"))

(after! lsp
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("nil"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))
