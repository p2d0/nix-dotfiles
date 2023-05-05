;;; editors/.doom.d/configuration/autodarkmode.el -*- lexical-binding: t; -*-
(require 'dbus)
(defun get-color-scheme ()
	(car (car (dbus-call-method :session "org.freedesktop.portal.Desktop" "/org/freedesktop/portal/desktop" "org.freedesktop.portal.Settings"  "Read" "org.freedesktop.appearance" "color-scheme") )))

(defun theme--handle-dbus-event (a setting values)
  "Handler for FreeDesktop theme changes."
  (when (string= setting "color-scheme")
    (let ((scheme (car values)))
      (cond
        ((= 1 scheme)
					(custom-set-faces!
						;; '(window-divider   ((t (:foreground "#1E2029" :background "#1E2029"))))
						;; '(solaire-default-face   ((t (:background "#1E2029"))))
						;; '(internal-border   ((t (:foreground "#1E2029" :background "#1E2029"))))

						;; (window-divider :foreground "#1E2029" :background "#1E2029")
						;; (solaire-default-face :background "#1E2029")
						;; (internal-border :foreground "#1E2029" :background "#1E2029")
						;; (doom-nano-modeline-evil-emacs-state-face    :foreground "#1E2029" :background "#1E2029")
						;; (doom-nano-modeline-evil-insert-state-face   :foreground "#1E2029" :background "#1E2029")
						;; (doom-nano-modeline-evil-motion-state-face   :foreground "#1E2029" :background "#1E2029")
						;; (doom-nano-modeline-evil-normal-state-face   :foreground "#1E2029" :background "#1E2029")
						;; (doom-nano-modeline-evil-operator-state-face :foreground "#1E2029" :background "#1E2029")
						;; (doom-nano-modeline-evil-replace-state-face  :foreground "#1E2029" :background "#1E2029")
						;; (doom-nano-modeline-evil-visual-state-face   :foreground "#1E2029" :background "#1E2029")
						;; (doom-nano-modeline-inactive-face            :foreground "#1E2029" :background "#1E2029")
						)

          (load-theme +dark-theme+ t)

					) ;; my custom function that sets a dark theme
        ((= 2 scheme)
          (load-theme +light-theme+ t)) ;; 1000 internet points to whoever guesses what this does
        (t (message "I don't know how to handle scheme: %s" scheme))))))

(dbus-register-signal :session
  "org.freedesktop.portal"
  "/org/freedesktop/portal/desktop"
  "org.freedesktop.impl.portal.Settings"
  "SettingChanged"
  #'theme--handle-dbus-event)

