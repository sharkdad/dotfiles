(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

(setq read-process-output-max (* 512 1024))

(set-language-environment "UTF-8")
(setq default-input-method nil)

(setq load-prefer-newer t)
(setq native-comp-jit-compilation t)

(setq package-native-compile t)

(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-warning-on-missing-source nil)

(setq byte-compile-warnings nil)
(setq byte-compile-verbose nil)

(setq inhibit-compacting-font-caches t)

(setq auto-mode-case-fold nil)

(setq inhibit-x-resources t)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq idle-update-delay 1.0)

(setq fast-but-imprecise-scrolling t)

(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

;;; frame

(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)
(setq window-divider-default-bottom-width 1
      window-divider-default-places t
      window-divider-default-right-width 1)

(setq frame-title-format "%b – GNU Emacs"
      icon-title-format "%b – GNU Emacs")

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(load-theme 'modus-vivendi t)
