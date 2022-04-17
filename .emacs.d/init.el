;; Visual

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-fringes 'subtle)

(load-theme 'modus-vivendi)

(column-number-mode t)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(set-frame-font "Input-10" nil t)
(set-fringe-mode 20)
(tool-bar-mode 0)
(tooltip-mode 0)

(setq default-frame-alist '((undecorated . t)))
(setq inhibit-startup-message t)
(setq visible-bell t)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Behavior

(with-eval-after-load 'dired
  (require 'dired-x)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(global-auto-revert-mode t)

(setq auto-save-timeout 5)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq bookmark-save-flag 1)
(setq common-lisp-style-default "basic")
(setq custom-file null-device)
(setq delete-old-versions t)
(setq inferior-lisp-program "sbcl")
(setq kept-new-versions 10)
(setq kept-old-versions 0)
(setq vc-make-backup-files t)
(setq version-control t)

(global-set-key (kbd "<escape>") 'keyboard-quit)

;; Packages

(require 'package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package diminish)

(use-package company
  :diminish
  :config
  (setq company-idle-delay 0.5)
  (global-company-mode t))

(use-package counsel
  :diminish counsel-mode
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t))

(use-package sly)

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (smartparens-global-strict-mode t)
  (sp-use-smartparens-bindings))
