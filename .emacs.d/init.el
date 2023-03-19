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

;; dired

(with-eval-after-load 'dired
  (require 'dired-x)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (setq dired-kill-when-opening-new-dired-buffer t))

;; Languages

(setq common-lisp-style-default "basic")
(setq inferior-lisp-program "sbcl")

;; Behavior

(setq auto-save-timeout 5)
(setq bookmark-save-flag 1)
(setq completion-ignore-case t)
(setq custom-file null-device)
(setq make-backup-files nil)

(global-auto-revert-mode t)
(savehist-mode t)

(setq enable-recursive-minibuffers t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Packages

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package diminish)

(use-package vertico
  :init
  (vertico-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (smartparens-global-mode t)
  (smartparens-global-strict-mode t))

(use-package which-key
  :demand
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package magit)
