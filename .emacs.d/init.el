;; Visual

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-fringes 'subtle)

(load-theme 'modus-vivendi)

(column-number-mode t)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(set-frame-font "Input-14" nil t)
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
(savehist-mode t)

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
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c p") 'previous-buffer)
(global-set-key (kbd "C-c n") 'next-buffer)

;; Packages

(require 'package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package amx)

(use-package avy
  :demand
  :bind (("C-;" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-g g" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1)
	 ("M-g e" . avy-goto-word-0)))

(use-package diminish)

(use-package company
  :diminish
  :config
  (setq company-idle-delay 0.5)
  (global-company-mode t))

(use-package counsel
  :demand
  :diminish counsel-mode
  :diminish ivy-mode
  :bind (("C-c s" . swiper)
	 ("C-x C-r" . counsel-recentf))
  :config
  (setq ivy-height 15)
  (ivy-mode t)
  (counsel-mode t))

(use-package flx)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package sly)

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (smartparens-global-strict-mode t)
  (sp-use-smartparens-bindings))
