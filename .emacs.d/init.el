;; Visual

(column-number-mode t)

(global-hl-line-mode 1)
(global-visual-line-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(set-frame-font "Monospace-11" nil t)
(set-fringe-mode 12)
(tool-bar-mode 0)

(setq even-window-heights nil)
(setq frame-resize-pixelwise t)
(setq inhibit-startup-message t)
(setq initial-buffer-choice 'recover-session)
(setq switch-to-buffer-obey-display-actions t)
(setq visible-bell t)

(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

;; Languages

(setq common-lisp-style-default "basic")
(setq inferior-lisp-program "sbcl")

;; Behavior

(setq auto-save-timeout 5)
(setq bookmark-save-flag 1)
(setq completion-ignore-case t)
(setq custom-file null-device)
(setq make-backup-files nil)
(setq switch-to-buffer-obey-display-actions t)
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

(with-eval-after-load 'dired
  (require 'dired-x)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (setq dired-kill-when-opening-new-dired-buffer t))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Global keybinds

;; display-buffer
;; switch-to-buffer

(bind-keys*
 ("M-o" . other-window)
 ("C-x k" . kill-current-buffer)
 ("C-x C-b" . ibuffer)
 ("C-c 8" . set-80-columns))


(use-package night-owl-theme
  :config
  (load-theme 'night-owl t))

(use-package diminish)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package magit)

(use-package corfu
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  :init
  (global-corfu-mode))

(use-package vertico
  :demand
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode)
  (setq vertico-count 20))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

