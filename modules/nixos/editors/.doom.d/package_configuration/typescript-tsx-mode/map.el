;;; package_configuration/typescript-tsx-mode/map.el -*- lexical-binding: t; -*-

;; Disable autocomplete hotkey binding for autocomplete to work in tsx mode
(map!
  :map typescript-tsx-mode-map
  "M-/" nil)
