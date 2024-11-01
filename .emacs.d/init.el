;;; init.el -*- lexical-binding: t; -*-


;;; custom file

(setq custom-safe-themes t)
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file) (load custom-file))


;;; frame

(menu-bar-mode 0)
(scroll-bar-mode 0)
(set-fringe-mode (max 8 (frame-char-width)))
(tool-bar-mode 0)

(setq frame-resize-pixelwise t)

(let ((frame (selected-frame)))
  (select-frame-set-input-focus frame)
  (let ((height (frame-height frame)))
    (set-frame-parameter frame 'fullscreen nil)
    (set-frame-height frame height)))


;;; package system

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(require 'use-package-ensure)
(setq use-package-always-ensure t)


;;; ui, display

(column-number-mode)

(global-hl-line-mode)
(show-paren-mode)

(defun my/prog-hook ()
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'my/prog-hook)

(setq display-buffer-alist
      '((t (display-buffer-reuse-window display-buffer-in-previous-window))))
(setq help-window-select t)
(setq inhibit-startup-message t)
(setq initial-buffer-choice #'recover-session)
(setq switch-to-buffer-obey-display-actions t)
(setq visible-bell t)
(setq warning-minimum-level :error)

(setq split-window-preferred-function #'my/split-window-only-once)

(defun my/split-window-only-once (window)
  (let* ((lru (get-lru-window nil nil nil t))
         (mru (get-mru-window nil nil nil t)))
    (when (eq lru mru) (split-window-sensibly window))))

(winner-mode t)
(bind-keys ("C-<tab>"         . winner-undo)
           ("C-<iso-lefttab>" . winner-redo))

(defun my/move-to-other-window ()
  (interactive)
  (let* ((window (selected-window))
         (buffer (current-buffer))
         (other (display-buffer buffer t)))
    (when other
      (quit-window nil window)
      (select-window other))))

(bind-keys ("C-c o" . my/move-to-other-window)
           ("C-c q" . quit-window)
           ("C-c w b" . balance-windows)
           ("C-c w m" . maximize-window)
           :repeat-map my/move-to-other-window-repeat-map
           ("o"     . my/move-to-other-window))

(use-package delight)
(use-package ef-themes)
(use-package mode-line-bell :config (mode-line-bell-mode))
(use-package rainbow-delimiters :hook prog-mode)

(use-package which-key
  :delight
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-setup-minibuffer)
  (which-key-mode))


;;; misc behavior

(electric-pair-mode)
(global-auto-revert-mode)
(global-goto-address-mode)
(global-subword-mode)
(repeat-mode)

(setq bookmark-save-flag 1)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq set-mark-command-repeat-pop t)
(setq use-short-answers t)
(setq vc-follow-symlinks t)

(setq-default buffer-file-coding-system 'utf-8-unix)

(setq kill-whole-line t)
(setq line-move-visual nil)
(setq-default word-wrap t)

(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq auto-save-timeout 5)
(setq make-backup-files nil)
(setq kill-buffer-delete-auto-save-files t)

(bind-keys ("C-x k"   . kill-current-buffer)
           ("C-x C-b" . ibuffer))

(use-package avy
  :bind
  (("C-;" . avy-goto-line)
   ("C-'" . avy-goto-char-2)))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))


;;; minibuffer, history, completion

(recentf-mode)
(savehist-mode)

(setq completion-ignore-case t)
(setq history-length 10000)
(setq read-extended-command-predicate #'command-completion-default-include-p)
(setq recentf-max-saved-items 1000)

(use-package consult
  :bind
  (([remap Info-search] . consult-info)
   ("C-c M-x"           . consult-mode-command)
   ("C-c h"             . consult-history)
   ("C-c k"             . consult-kmacro)
   ("C-c m"             . consult-man)
   ("C-c i"             . consult-info)
   ("C-x M-:"           . consult-complex-command)
   ("C-x b"             . consult-buffer)
   ("C-x 4 b"           . consult-buffer-other-window)
   ("C-x 5 b"           . consult-buffer-other-frame)
   ("C-x r b"           . consult-bookmark)
   ("C-x p b"           . consult-project-buffer)
   ("M-#"               . consult-register-load)
   ("M-'"               . consult-register-store)
   ("C-M-#"             . consult-register)
   ("M-y"               . consult-yank-pop)
   ("M-g e"             . consult-compile-error)
   ("M-g f"             . consult-flymake)
   ("M-g g"             . consult-goto-line)
   ("M-g M-g"           . consult-goto-line)
   ("M-g o"             . consult-outline)
   ("M-g m"             . consult-mark)
   ("M-g M"             . consult-global-mark)
   ("M-g i"             . consult-imenu)
   ("M-g I"             . consult-imenu-multi)
   ("M-s f"             . consult-find)
   ("M-s g"             . consult-grep)
   ("M-s r"             . consult-ripgrep)
   ("M-s l"             . consult-line)
   ("M-s L"             . consult-line-multi)
   ("M-s e"             . consult-isearch-history)
   :map isearch-mode-map
   ("M-e"               . consult-isearch-history)
   ("M-s e"             . consult-isearch-history)
   ("M-s l"             . consult-line)
   ("M-s L"             . consult-line-multi)
   :map minibuffer-local-map
   ("M-s"               . consult-history)
   ("M-r"               . consult-history))

  :config
  (setq consult-narrow-key "<")

  (setq register-preview-delay nil)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (consult-customize
   consult-theme
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-."))


(use-package corfu
  :demand
  :bind
  (:map corfu-map
        ("RET" . nil)
        ("C-;" . corfu-quick-jump))

  :config
  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  (setq corfu-count 20)
  (setq corfu-popupinfo-delay 0.5)
  (setq corfu-preselect 'first)
  (setq corfu-preview-current nil)

  (global-corfu-mode)

  (corfu-history-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  (corfu-popupinfo-mode))


(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)))


(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))


(use-package marginalia
  :config
  (marginalia-mode))


(use-package orderless
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion))
                                        (eglot (styles orderless basic)))))


(use-package vertico
  :demand
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind
  (:map vertico-map
        ("RET"   . vertico-directory-enter)
        ("DEL"   . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)
        ("C-;"   . vertico-quick-jump))

  :config
  (setq vertico-count (/ (window-total-height) 2))
  (vertico-mode))


;;; os

;; FIXME: what do
(setq tramp-histfile-override nil)

(setq ansi-color-for-comint-mode t)
(setq comint-input-ring-size history-length)
(setq comint-prompt-read-only t)
(setq comint-terminfo-terminal "dumb-emacs-ansi")
(setq shell-command-prompt-show-cwd t)

(setq-default comint-scroll-to-bottom-on-input t)

(add-hook 'comint-output-filter-functions #'ansi-color-process-output)


(use-package compile
  :ensure nil
  :config
  (setq compilation-message-face nil)
  (add-to-list 'compilation-error-regexp-alist 'shadow-cljs)
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(shadow-cljs "^-+ \\(ERROR\\|WARNING\\) .*?-+\n File: \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 2 3 4)))


(advice-add 'comint-term-environment :filter-return #'my/comint-env)

(defun my/comint-env (env)
  (nconc (list "PAGER=cat" "COLORTERM=truecolor") env))


(defvar-local my/comint-history-variable nil)
(advice-add 'comint-add-to-input-history :after-while #'my/comint-add-history)

(defun my/comint-history-setup ()
  (setq my/comint-history-variable
        (intern (concat (symbol-name major-mode) "-history")))
  (add-to-list 'savehist-additional-variables my/comint-history-variable)

  (unless (boundp my/comint-history-variable)
    (set my/comint-history-variable nil))

  (when-let ((history (symbol-value my/comint-history-variable)))
    (setq-local comint-input-ring (ring-convert-sequence-to-ring history))))

(defun my/comint-add-history (cmd)
  (when my/comint-history-variable
    (add-to-history my/comint-history-variable (substring-no-properties cmd))))


(add-hook 'shell-mode-hook #'compilation-shell-minor-mode)
(add-hook 'shell-mode-hook #'my/comint-history-setup)
(add-hook 'shell-mode-hook #'my/shell-hook)

(defun my/shell-hook ()
  (setq-local comint-process-echoes t))



(defun my/term-default ()
  (interactive)
  (term shell-file-name))

(bind-keys ("C-c s" . shell)
           ("C-c t" . my/term-default))


(require 'dired-x)
(setq dired-dwim-target t)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(add-hook 'dired-mode-hook #'dired-omit-mode)


(setq eww-auto-rename-buffer 'title)
(setq shr-inhibit-images t)


(use-package org
  :ensure nil
  :bind
  (("C-c a" . org-agenda))

  :config
  (setq org-agenda-custom-commands
   '(("d" "dashboard" ((tags-todo "CATEGORY=\"inbox\"")
                       (agenda "")))
     ("b" "backlog" tags-todo "-fun")
     ("f" "fun" tags-todo "fun")))
  (setq org-agenda-dim-blocked-tasks 'invisible)
  (setq org-agenda-files '("~/sync/org/"))
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-todo-ignore-deadlines 'all)
  (setq org-agenda-todo-ignore-scheduled 'all)
  (setq org-agenda-window-setup 'current-window)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-log-done 'time)
  (setq org-tag-alist '(("fun" . ?f)))

  (add-to-list 'org-modules 'org-habit t))


(use-package org-roam
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n d" . org-roam-dailies-goto-today)
   :map org-mode-map
   ("C-c n i" . org-roam-node-insert)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n l" . org-roam-buffer-toggle))

  :config
  (setq org-roam-directory "~/sync/org/roam")
  (org-roam-db-autosync-mode))


;;; dev

(use-package dape
  :bind
  (("C-c d d" . dape))

  :init
  (setq dape-key-prefix "\C-cd")

  :config
  (setq dape-info-hide-mode-line nil)
  (remove-hook 'dape-on-start-hooks #'dape-info)
  (remove-hook 'dape-on-start-hooks #'dape-repl)
  (add-hook 'dape-on-start-hooks #'save-some-buffers)
  (add-hook 'dape-on-stopped-hooks #'dape-info)
  (advice-add 'dape--display-buffer :around #'my/dape-fix-display-buffer))

(defun my/dape-fix-display-buffer (orig-fun &rest args)
  (let ((display-buffer-alist `(((derived-mode . dape-info-parent-mode)
                                 (display-buffer-in-side-window)
                                 (side . bottom)
                                 (window-parameters
                                  (no-other-window . t))))))
    (apply orig-fun args)))


(use-package eldoc
  :ensure nil
  :delight
  :bind
  (("C-h ." . eldoc-doc-buffer))

  :config
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose)
  (setq eldoc-echo-area-prefer-doc-buffer t)
  (setq eldoc-echo-area-use-multiline-p 6))


(use-package eglot
  :hook (go-ts-mode       . eglot-ensure)
  :hook (python-base-mode . eglot-ensure)

  :config
  (setq eglot-autoshutdown t)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-events-buffer-size 0)
  (setq eglot-extend-to-xref t)
  (setq eglot-ignored-server-capabilities '(:colorProvider :inlayHintProvider))
  (setq eglot-report-progress nil)
  (setq eglot-sync-connect nil)
  (setq eglot-workspace-configuration
        '((:python\.analysis . (:diagnosticMode "workspace"))))

  (add-to-list 'eglot-server-programs '(glsl-ts-mode . ("glsl_analyzer"))))

(defun my/eglot-organize-imports ()
  (interactive)
  (eglot-code-actions nil nil "source.organizeImports" t))


(defun my/flymake-project ()
  (interactive)
  (flymake-show-project-diagnostics)
  (let* ((prj (project-current))
         (root (project-root prj))
         (buffer (flymake--project-diagnostics-buffer root)))
    (select-window (get-buffer-window buffer))))

(use-package flymake
  :ensure nil
  :bind
  (("C-c f" . my/flymake-project)
   :map prog-mode-map
   ("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)))


(use-package idle-highlight-mode
  :hook prog-mode)


(use-package magit
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch)
   ("C-c M-g" . magit-file-dispatch)
   :map magit-file-section-map
   ("RET"     . magit-diff-visit-file-other-window)
   :map magit-hunk-section-map
   ("RET"     . magit-diff-visit-file-other-window))

  :config
  (setq magit-diff-refine-hunk 'all)
  (setq magit-section-initial-visibility-alist '((stashes . hide)
                                                 (file . hide))))


(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-auto-langs '(css dockerfile go gomod html javascript
                                 json makefile markdown python toml
                                 tsx typescript yaml))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;;; languages

(setq c-default-style "java")


(use-package clojure-mode
  :defer t)

(defun my/cider-eldoc-docstring (orig-fun &rest args)
  (cl-destructuring-bind (thing pos eldoc-info) args
    (when-let ((orig-eldoc (apply orig-fun args)))
      (if-let ((docstring (lax-plist-get eldoc-info "docstring")))
          (format "%s\n\n%s"
                  orig-eldoc
                  (cider-docstring--trim (cider-docstring--format docstring)))
        orig-eldoc))))

(use-package cider
  :defer t
  :config
  (setq cider-eldoc-display-context-dependent-info t)
  (add-hook 'cider-repl-mode-hook #'compilation-shell-minor-mode)
  (advice-add 'cider-eldoc-format-function :around #'my/cider-eldoc-docstring)
  (advice-add 'cider-eldoc-format-special-form :around #'my/cider-eldoc-docstring))


(setq go-ts-mode-indent-offset tab-width)
(add-hook 'go-ts-mode-hook #'my/go-hook)

(defun my/go-hook ()
  (add-hook 'before-save-hook #'my/eglot-organize-imports nil t)
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))


(use-package markdown-mode
  :mode "\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'")


(use-package python
  :ensure nil
  :bind
  (:map python-base-mode-map
        ("TAB"       . my/python-indent-for-tab-command)
        ("<backtab>" . python-indent-shift-left))
  :config
  (add-hook 'inferior-python-mode-hook #'my/comint-history-setup)
  (add-hook 'python-base-mode-hook #'my/python-hook)

  (add-to-list 'python-indent-trigger-commands
               #'my/python-indent-for-tab-command))

(defun my/python-hook ()
  (setq-local tab-always-indent t))

(defun my/python-indent-for-tab-command ()
  (interactive)
  (cond ((use-region-p)
         (call-interactively 'python-indent-shift-right))
        ((<= (current-column) (current-indentation))
         (call-interactively 'indent-for-tab-command))
        (t
         (call-interactively 'completion-at-point))))


(use-package pet
  :delight
  :init
  (add-hook 'python-base-mode-hook 'pet-mode -10))


(use-package squirrel-mode
  :mode "\\.nut\\'"
  :config
  (add-hook 'squirrel-mode-hook #'my/squirrel-hook))

(defun my/squirrel-hook ()
  (setq-local indent-tabs-mode t))


(use-package tidal
  :mode "\\.tidal\\'")


(use-package web-mode
  :hook html-mode
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-comment-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(defun my/web-hook ()
  (setq-local electric-indent-inhibit t)
  (setq-local tab-width 2)
  (electric-pair-local-mode -1)
  (web-mode-set-engine "django"))

