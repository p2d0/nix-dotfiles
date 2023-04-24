;;; ~/.doom.d/package_configuration/csharp-mode.el -*- lexical-binding: t; -*-

(defun csharp-disable-clear-string-fences (orig-fun &rest args)
  "This turns off `c-clear-string-fences' for `csharp-mode'. When
on for `csharp-mode' font lock breaks after an interpolated string
or terminating simple string."
  (unless (equal major-mode 'csharp-mode)
    (apply orig-fun args)))

(advice-add 'c-clear-string-fences :around 'csharp-disable-clear-string-fences)

(add-to-list 'auto-mode-alist '("\\.cshtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.csproj$" . xml-mode))
