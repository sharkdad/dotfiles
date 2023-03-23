;; Visual

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-fringes 'subtle)
(load-theme 'modus-vivendi)

(column-number-mode t)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(set-frame-font "Cascadia Mono-12" nil t)
(set-fringe-mode 12)
(tool-bar-mode 0)

(setq even-window-heights nil)
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
(setq vc-follow-symlinks t)

(global-auto-revert-mode t)
(electric-pair-mode t)
(savehist-mode t)

;; Allow using minibuffer when in minibuffer
(setq enable-recursive-minibuffers t)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Hide commands in M-x which do not work in the current mode.
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

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(use-package diminish)

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  (setq vertico-count 20))

(use-package vertico-directory
  :straight nil
  :after vertico
  :demand
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package which-key
  :demand
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package magit)
