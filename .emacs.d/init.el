;;; init.el -*- lexical-binding: t; -*-


;;; package system

(require 'package)
(require 'use-package-ensure)
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("elpa"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(setq use-package-always-demand t)
(setq use-package-always-ensure t)


;;; custom file

(setq custom-safe-themes t)
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file) (load custom-file))


;;; ui, display

(blink-cursor-mode -1)

(column-number-mode)

(global-hl-line-mode)

(set-fringe-mode (frame-char-width))

(show-paren-mode)
(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(setq blink-matching-paren nil)

(defun my/prog-hook ()
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'my/prog-hook)

(defun my/window-resize-golden (&optional window)
  (interactive)
  (let* ((window (window-normalize-window window))
         (parent (window-parent window))
         (horiz (window-left-child parent))
         (parent-size (window-size parent horiz))
         (curr-size (window-size window horiz))
         (min-size (if horiz window-min-width window-min-height))
         (max-size (- parent-size min-size))
         (new-size (min max-size (* 2 (/ parent-size 3)))))
    (window-resize window (- new-size curr-size) horiz)))

(setq display-buffer-overriding-action
      '((display-buffer-reuse-window
         display-buffer-in-previous-window)))
(setq display-buffer-base-action
      '((display-buffer-use-some-window)))
(setq even-window-sizes nil)
(setq switch-to-buffer-obey-display-actions t)

(setq ad-redefinition-action 'accept)
(setq help-window-select t)
(setq inhibit-startup-message t)
(setq initial-buffer-choice #'recover-session)
(setq scroll-conservatively 10)
(setq visible-bell t)
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

(use-package winner
  :ensure nil
  :bind
  (("C-<tab>"         . winner-undo)
   ("C-<iso-lefttab>" . winner-redo))
  :config
  (winner-mode))

(defun my/move-to-other-window ()
  (interactive)
  (let* ((buffer (current-buffer))
         (other (display-buffer buffer t)))
    (switch-to-prev-buffer nil t)
    (select-window other)))

(bind-keys ("C-c o" . my/move-to-other-window)
           ("C-x w b" . balance-windows)
           ("C-x w g" . my/window-resize-golden)
           ("C-x w m" . toggle-frame-maximized)
           :repeat-map my/move-to-other-window-repeat-map
           ("o"     . my/move-to-other-window))

(use-package delight)
(use-package ef-themes)
(use-package mode-line-bell :config (mode-line-bell-mode))

(setq rainbow-delimiters-max-face-count 5)
(use-package rainbow-delimiters :hook prog-mode)

(use-package which-key
  :delight
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))


;;; misc behavior

(electric-pair-mode)
(global-auto-revert-mode)
(global-goto-address-mode)
(global-subword-mode)
(repeat-mode)

(setq bookmark-save-flag 1)
(setq ffap-machine-p-known 'reject)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq save-interprogram-paste-before-kill 4096)
(setq set-mark-command-repeat-pop t)
(setq use-short-answers t)
(setq vc-follow-symlinks t)

(setq-default buffer-file-coding-system 'utf-8-unix)

(setq kill-whole-line t)
(setq line-move-visual nil)
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(setq-default word-wrap t)

(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq auto-save-include-big-deletions t)
(setq auto-save-timeout 5)
(setq make-backup-files nil)
(setq kill-buffer-delete-auto-save-files t)

(defun my/quit-and-kill-window ()
  (interactive)
  (quit-window t))

(bind-keys ("C-x k"   . my/quit-and-kill-window)
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

  (setq text-mode-ispell-word-completion nil)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (consult-customize
   consult-theme
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-."))

(use-package consult-eglot
  :after consult
  :bind
  ("M-g s" . consult-eglot-symbols))


(use-package corfu
  :demand t
  :config
  (setq corfu-map
        (define-keymap
          "<remap> <completion-at-point>" #'corfu-complete
          "<remap> <keyboard-escape-quit>" #'corfu-reset
          "M-n" #'corfu-next
          "M-p" #'corfu-previous
          "C-g" #'corfu-quit
          "TAB" #'corfu-complete
          "M-g" #'corfu-info-location
          "M-h" #'corfu-info-documentation
          "M-SPC" #'corfu-insert-separator
          "C-M-;" #'corfu-quick-jump))

  (setq corfu-auto t)
  (setq corfu-preselect 'first)
  (setq corfu-preview-current nil)
  (global-corfu-mode)

  (corfu-history-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  (setq corfu-popupinfo-delay 0.5)
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


(use-package pcmpl-args)


(use-package vertico
  :demand t
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind
  (:map vertico-map
        ("RET"   . vertico-directory-enter)
        ("DEL"   . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)
        ("C-;"   . vertico-quick-jump))

  :config
  (vertico-mode))


;;; os

;; FIXME: what do
(setq tramp-histfile-override nil)

(setq comint-input-ring-size history-length)
(setq shell-command-prompt-show-cwd t)

(setq-default comint-scroll-to-bottom-on-input t)


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


(use-package dired
  :ensure nil
  :config
  (require 'dired-x)
  (setq dired-dwim-target t)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-clean-confirm-killing-deleted-buffers nil
        dired-recursive-copies 'always
        dired-create-destination-dirs 'ask))


(use-package eat
  :hook (eshell-load . eat-eshell-mode)
  :bind
  (("C-x p t" . eat-project))
  :config
  (require 'pcmpl-args)
  (setq eat-enable-auto-line-mode t)
  (setq eshell-visual-commands '()))


(setq eww-auto-rename-buffer 'title)
(setq shr-inhibit-images t)


(use-package org
  :ensure nil
  :bind
  (("C-c a" . org-agenda))

  :config
  (setq org-agenda-dim-blocked-tasks 'invisible)
  (setq org-agenda-files '("~/sync/org/todo.org"))
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-todo-ignore-deadlines 'all)
  (setq org-agenda-todo-ignore-scheduled 'all)
  (setq org-agenda-window-setup 'current-window)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-log-done 'time)
  (setq org-startup-truncated nil)

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

(setq project-vc-extra-root-markers '("package-lock.json" ".venv"))

(use-package dape
  :bind
  (("C-c d d" . dape))

  :init
  (setq dape-key-prefix "\C-cd")

  :config
  (setq dape-buffer-window-arrangement nil)
  (setq dape-info-hide-mode-line nil)
  (setq dape-repl-echo-shell-output t)
  (remove-hook 'dape-on-start-hooks #'dape-info)
  (add-hook 'dape-on-start-hooks #'save-some-buffers)
  (add-hook 'dape-repl-mode-hook #'compilation-shell-minor-mode))


(use-package dash-docs
  :config
  (setq dash-docs-browser-func 'eww))

(use-package consult-dash
  :bind (("M-s d" . consult-dash))
  :config
  (consult-customize consult-dash :initial (thing-at-point 'symbol)))


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

  :bind
  (("C-c c d" . eglot-find-declaration)
   ("C-c c t" . eglot-find-typeDefinition)
   ("C-c c r" . eglot-rename))

  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)
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


(use-package clojure-mode)

(use-package cider
  :config
  (setq cider-clojure-cli-aliases ":dev:test")
  (setq cider-eldoc-display-context-dependent-info t)
  (setq cider-enable-nrepl-jvmti-agent t))


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
  (setq python-indent-guess-indent-offset-verbose nil)

  (add-hook 'inferior-python-mode-hook #'my/comint-history-setup)

  (add-hook 'python-base-mode-hook #'pet-mode -10)
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
  :delight)


(use-package squirrel-mode
  :mode "\\.nut\\'"
  :config
  (add-hook 'squirrel-mode-hook #'my/squirrel-hook))

(defun my/squirrel-hook ()
  (setq-local indent-tabs-mode t))


(use-package tidal
  :defer t
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

