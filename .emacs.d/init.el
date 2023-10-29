;; Visual

(column-number-mode t)

(global-hl-line-mode 1)
(global-visual-line-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(set-frame-font "Cascadia Mono-13" nil t)
(set-fringe-mode 12)
(tool-bar-mode 0)

(setq display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-in-previous-window)))
(setq even-window-heights nil)
(setq frame-resize-pixelwise t)
(setq inhibit-startup-message t)
(setq initial-buffer-choice 'recover-session)
(setq split-width-threshold 200)
(setq switch-to-buffer-obey-display-actions t)
(setq visible-bell t)

(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(defun move-buffer-other-window ()
  "Put the buffer from the selected window in other window"
  (interactive)
  (let* ((buffer (window-buffer))
     (other-window (next-window)))
    (quit-window)
    (select-window other-window)
    (switch-to-buffer buffer)))

(defun toggle-maximize ()
  "Kill other windows or winner-undo"
  (interactive)
  (if (frame-root-window-p (get-buffer-window))
      (winner-undo)
    (delete-other-windows)))

;; Languages

(setq common-lisp-style-default "basic")
(setq inferior-lisp-program "sbcl")

;; Behavior

(setq auto-save-timeout 5)
(setq bookmark-save-flag 1)
(setq completion-ignore-case t)
(setq custom-file null-device)
(setq kill-whole-line 1)
(setq make-backup-files nil)
(setq vc-follow-symlinks t)

(global-auto-revert-mode t)
(electric-pair-mode t)
(repeat-mode t)
(savehist-mode t)
(winner-mode t)

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
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$"))
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode))))

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

(bind-keys
 ("M-o" . other-window)
 ("M-`" . toggle-maximize)
 ("C-x k" . kill-current-buffer)
 ("C-x C-b" . ibuffer)
 ("C-c 8" . set-80-columns)
 ("C-c o" . move-buffer-other-window))

(use-package ef-themes
  :config
  (load-theme 'ef-night t))

(use-package night-owl-theme)

(use-package diminish)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package magit
  :init
  (setq magit-diff-refine-hunk 'all))

(use-package corfu
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  :init
  (global-corfu-mode)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package vertico
  :demand
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  (setq vertico-count 30))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<"))

(use-package eglot)

(use-package tree-sitter)

(use-package tree-sitter-langs)
