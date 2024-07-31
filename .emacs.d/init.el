;;; init.el -*- lexical-binding: t; -*-


;;; custom file

(setq custom-safe-themes t)
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file) (load custom-file))


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
(set-fringe-mode (max 8 (frame-char-width)))

(global-hl-line-mode)
(show-paren-mode)

(defun my/show-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'my/show-trailing-whitespace)

(setq display-buffer-alist
      '((t (display-buffer-reuse-window display-buffer-in-previous-window))))
(setq inhibit-startup-message t)
(setq initial-buffer-choice #'recover-session)
(setq switch-to-buffer-obey-display-actions t)
(setq warning-minimum-level :error)

(setq even-window-sizes nil)
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
           :repeat-map my/move-to-other-window-repeat-map
           ("o"     . my/move-to-other-window))

(use-package delight)
(use-package ef-themes)
(use-package mode-line-bell :config (mode-line-bell-mode))
(use-package rainbow-delimiters :hook prog-mode)

(use-package emacs
  :ensure nil
  :delight
  (eldoc-mode)
  (global-superword-mode)
  (superword-mode))

(use-package which-key
  :delight

  :custom
  (which-key-idle-delay 0.5)

  :config
  (which-key-setup-minibuffer)
  (which-key-mode))


;;; misc behavior

(electric-pair-mode)
(global-auto-revert-mode)
(global-goto-address-mode)
(global-superword-mode)
(repeat-mode)

(setq bookmark-save-flag 1)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
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

  :custom
  (consult-narrow-key "<")
  (register-preview-delay nil)
  (register-preview-function #'consult-register-format)

  (xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (advice-add #'register-preview :override #'consult-register-window)

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
        ("RET" . nil))

  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-delay 0.5)

  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history))


(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)))


(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))


(use-package marginalia
  :init
  (marginalia-mode))


(use-package orderless
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))


(use-package vertico
  :demand
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind
  (:map vertico-map
        ("RET"   . vertico-directory-enter)
        ("DEL"   . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)
        ("C-;"   . vertico-quick-jump))

  :custom
  (vertico-count (/ (window-total-height) 2))

  :config
  (vertico-mode))


;;; os

;; FIXME: what do
(setq tramp-histfile-override nil)

(setq ansi-color-for-comint-mode t)
(setq comint-prompt-read-only t)
(setq comint-terminfo-terminal "dumb-emacs-ansi")
(setq shell-command-prompt-show-cwd t)

(setq-default comint-scroll-to-bottom-on-input t)

(advice-add 'comint-add-to-input-history :after-while #'my/shell-add-history)
(add-hook 'comint-output-filter-functions #'ansi-color-process-output)
(add-hook 'shell-mode-hook #'compilation-shell-minor-mode)
(add-hook 'shell-mode-hook #'my/shell-hook)

(defun my/shell-add-history (cmd)
  (add-to-history 'shell-command-history (substring-no-properties cmd)))

(defun my/shell-hook ()
  (setq-local comint-process-echoes t)

  (setq-local comint-input-ring (make-ring (length shell-command-history)))
  (dolist (cmd shell-command-history)
    (ring-insert-at-beginning comint-input-ring cmd)))

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


(use-package org
  :ensure nil
  :bind
  (("C-c a" . org-agenda))

  :custom
  (org-agenda-custom-commands
   '(("d" "dashboard" ((tags-todo "CATEGORY=\"inbox\"")
                       (agenda "")))
     ("b" "backlog" tags-todo "-fun")
     ("f" "fun" tags-todo "fun")))
  (org-agenda-dim-blocked-tasks 'invisible)
  (org-agenda-files '("~/sync/org/"))
  (org-agenda-start-on-weekday nil)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-todo-ignore-deadlines 'all)
  (org-agenda-todo-ignore-scheduled 'all)
  (org-agenda-window-setup 'current-window)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-log-done 'time)
  (org-tag-alist '(("fun" . ?f)))

  :config
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

  :custom
  (org-roam-directory "~/sync/org/roam")

  :config
  (org-roam-db-autosync-mode))


;;; dev

(use-package dape
  :bind
  (("C-c d d" . dape))

  :init
  (setq dape-key-prefix "\C-cd")

  :custom
  (dape-info-hide-mode-line nil)

  :config
  (remove-hook 'dape-on-start-hooks #'dape-info)
  (remove-hook 'dape-on-start-hooks #'dape-repl)
  (add-hook 'dape-on-start-hooks #'save-some-buffers)
  (add-hook 'dape-on-stopped-hooks #'dape-info)
  (advice-add 'dape--display-buffer :around #'my/dape-fix-display-buffer)

  (defun my/dape-fix-display-buffer (orig-fun &rest args)
    (let ((display-buffer-alist `(((derived-mode . dape-info-parent-mode)
                                   (display-buffer-in-side-window)
                                   (side . bottom)
                                   (window-parameters
                                    (no-other-window . t))))))
      (apply orig-fun args))))


(use-package eldoc-box
  :delight (eldoc-box-hover-at-point-mode)
  :hook (eldoc-mode . eldoc-box-hover-at-point-mode)
  :bind
  (("C-h ." . eldoc-doc-buffer))


  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)

  :config
  (setq eldoc-box-at-point-position-function #'my/eldoc-box--at-point-position)

  (defun my/eldoc-box--at-point-position (w h)
    (let* ((window-l (nth 0 (window-absolute-pixel-edges)))
           (window-r (nth 2 (window-absolute-pixel-edges)))
           (frame-l (nth 0 (frame-edges)))
           (frame-r (nth 2 (frame-edges)))
           (distance-l (- window-l frame-l))
           (distance-r (- frame-r window-r))
           (at-point-pos (eldoc-box--default-at-point-position-function w h)))
      (cond ((>= distance-l w) (cons (- window-l w) (cdr at-point-pos)))
            ((>= distance-r w) (cons window-r (cdr at-point-pos)))
            (t at-point-pos)))))


(use-package eglot
  :ensure nil
  :hook (go-ts-mode         . eglot-ensure)
  :hook (python-base-mode   . eglot-ensure)
  :hook (eglot-managed-mode . my/eglot-hook)

  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:colorProvider :inlayHintProvider))
  (eglot-report-progress nil)
  (eglot-workspace-configuration
   '((:python\.analysis . (:diagnosticMode "workspace"))))

  :config
  (setq completion-category-defaults nil)
  (add-to-list 'eglot-server-programs '(glsl-ts-mode . ("glsl_analyzer")))

  (defun my/eglot-hook ()
    (remove-hook 'eldoc-documentation-functions #'flymake-eldoc-function t)
    (add-hook 'eldoc-documentation-functions #'flymake-eldoc-function nil t))

  (defun my/eglot-organize-imports ()
    (interactive)
	(eglot-code-actions nil nil "source.organizeImports" t)))


(bind-keys ("C-c f" . my/flymake-project))

(defun my/flymake-project ()
  (interactive)
  (flymake-show-project-diagnostics)
  (let* ((prj (project-current))
         (root (project-root prj))
         (buffer (flymake--project-diagnostics-buffer root)))
    (switch-to-buffer buffer)))


(use-package magit
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch)
   ("C-c M-g" . magit-file-dispatch)
   :map magit-file-section-map
   ("RET"     . magit-diff-visit-file-other-window)
   :map magit-hunk-section-map
   ("RET"     . magit-diff-visit-file-other-window))

  :custom
  (magit-diff-refine-hunk 'all)
  (magit-section-initial-visibility-alist '((stashes . hide)
                                            (file . hide))))


(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(css dockerfile go gomod html javascript json
                        makefile markdown python toml tsx typescript
                        yaml))

  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;;; languages

(setq c-default-style "java")

(defun my/emacs-lisp-hook ()
  (remove-hook 'eldoc-documentation-functions #'elisp-eldoc-var-docstring t)
  (add-hook 'eldoc-documentation-functions #'elisp-eldoc-var-docstring-with-value nil t))
(add-hook 'emacs-lisp-mode-hook #'my/emacs-lisp-hook)


(use-package go-ts-mode
  :ensure nil
  :hook (go-ts-mode . my/go-hook)

  :custom
  (go-ts-mode-indent-offset (tab-width))

  :config
  (defun my/go-hook ()
    (add-hook 'before-save-hook #'my/eglot-organize-imports nil t)
    (add-hook 'before-save-hook #'eglot-format-buffer nil t)))


(use-package markdown-mode
  :mode "\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'")


(use-package python
  :ensure nil
  :hook (python-base-mode . my/python-hook)
  :hook (python-base-mode . eglot-ensure)

  :bind
  (:map python-base-mode-map
        ("<tab>"           . my/python-indent-for-tab-command)
        ("S-<iso-lefttab>" . python-indent-shift-left))

  :config
  (defun my/python-hook ()
    (setq-local tab-always-indent t))

  (defun my/python-indent-for-tab-command ()
    (interactive)
    (cond ((use-region-p)
           (call-interactively 'python-indent-shift-right))
          ((eq (current-indentation)
               (- (line-end-position) (line-beginning-position)))
           (call-interactively 'indent-for-tab-command))
          (t
           (call-interactively 'completion-at-point))))

  (add-to-list 'python-indent-trigger-commands
               #'my/python-indent-for-tab-command))


(use-package pet
  :delight
  :hook python-base-mode)


(use-package squirrel-mode
  :mode "\\.nut\\'"
  :hook (squirrel-mode . my/squirrel-hook)

  :config
  (defun my/squirrel-hook ()
    (setq-local indent-tabs-mode t)))


(use-package tidal
  :mode "\\.tidal\\'")


(use-package web-mode
  :hook html-mode
  :hook (web-mode . my/web-hook)

  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-comment-indent-offset 2)
  (web-mode-markup-indent-offset 2)

  :config
  (defun my/web-hook ()
    (setq-local electric-indent-inhibit t)
    (setq-local tab-width 2)
    (electric-pair-local-mode -1)
    (web-mode-set-engine "django")))
