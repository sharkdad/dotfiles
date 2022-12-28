;; Visual

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-fringes 'subtle)
(load-theme 'modus-vivendi)

(column-number-mode t)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(set-frame-font "Cascadia Mono-10" nil t)
(set-fringe-mode 12)
(tool-bar-mode 0)

(setq inhibit-startup-message t)
(setq initial-buffer-choice 'recover-session)
(setq visible-bell t)

(global-tab-line-mode)
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)
(setq tab-line-close-button-show nil)
(setq tab-line-new-button-show nil)

;; Behavior

(setq bookmark-save-flag 1)
(setq completion-ignore-case t)
(setq custom-file null-device)
(setq kill-whole-line 1)

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(global-auto-revert-mode t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(savehist-mode t)

;; dired

(with-eval-after-load 'dired
  (require 'dired-x)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (setq dired-kill-when-opening-new-dired-buffer t))

;; Languages

(setq common-lisp-style-default "basic")
(setq inferior-lisp-program "sbcl")

;; Backups

(setq auto-save-timeout 5)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq create-lockfiles nil)
(setq delete-old-versions t)
(setq kept-new-versions 50)
(setq kept-old-versions 0)
(setq vc-make-backup-files t)
(setq version-control t)

;; Packages

(require 'package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package amx)

(use-package avy
  :bind (("C-;" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-g g" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1)
	 ("M-g e" . avy-goto-word-0)))

(use-package diminish)

(use-package company
  :demand
  :diminish
  :config
  (setq company-idle-delay 0.5)
  (global-company-mode t))

(use-package counsel
  :demand
  :diminish counsel-mode
  :diminish ivy-mode
  :bind (("C-c f" . counsel-git)
         ("C-c s" . swiper))
  :config
  (setq counsel-find-file-ignore-regexp "\\(?:\\`\\|[/\\]\\)\\(?:[#.]\\)")
  (setq ivy-height 15)
  (setq ivy-use-virtual-buffers t)
  (ivy-mode t)
  (counsel-mode t))

(use-package css-eldoc
  :config
  (add-hook 'css-mode-hook 'turn-on-css-eldoc))

(use-package flx)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package slime
  :config
  (slime-setup '(slime-asdf slime-company slime-fancy)))

(use-package slime-company)

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (smartparens-global-mode t)
  (smartparens-global-strict-mode t)
  (sp-use-smartparens-bindings))

(use-package which-key
  :demand
  :diminish which-key-mode
  :bind (("C-h C-b" . which-key-show-top-level))
  :config
  (which-key-mode))

(use-package magit
  :bind (("C-c g" . magit-file-dispatch))
  :config
  (setq magit-diff-refine-hunk 'all))
