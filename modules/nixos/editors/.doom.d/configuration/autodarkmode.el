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
					(load-theme +dark-theme+ t)

					(custom-set-faces
						'(window-divider ((t (:foreground "#1E2029" :background "#1E2029"))) :override)
						'(solaire-default-face ((t (:background "#1E2029"))) :override)
						'(internal-border ((t (:foreground "#1E2029" :background "#1E2029"))) :override)
						'(doom-nano-modeline-evil-emacs-state-face ((t (:foreground "#1E2029" :background "#1E2029"))) :override)
						'(doom-nano-modeline-evil-insert-state-face ((t (:foreground "#1E2029" :background "#1E2029"))) :override)
						'(doom-nano-modeline-evil-motion-state-face ((t (:foreground "#1E2029" :background "#1E2029"))) :override)
						'(doom-nano-modeline-evil-normal-state-face ((t (:foreground "#1E2029" :background "#1E2029"))) :override)
						'(doom-nano-modeline-evil-operator-state-face ((t (:foreground "#1E2029" :background "#1E2029"))) :override)
						'(doom-nano-modeline-evil-replace-state-face ((t (:foreground "#1E2029" :background "#1E2029"))) :override)
						'(doom-nano-modeline-evil-visual-state-face ((t (:foreground "#1E2029" :background "#1E2029"))) :override)
						'(doom-nano-modeline-inactive-face ((t (:foreground "#1E202" :background "#1E2029"))) :override)
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

