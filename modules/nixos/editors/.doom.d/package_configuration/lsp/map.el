;;; package_configuration/lsp/map.el -*- lexical-binding: t; -*-

(defun indent-buffer ()
      (interactive)
      (save-excursion
        (indent-region (point-min) (point-max) nil)))
(map!
  :leader
  "c=" #'indent-buffer)
