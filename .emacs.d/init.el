;; -*- lexical-binding: t; -*-

;; Font

(let* ((host (downcase (system-name)))
       (font-size
        (pcase host
          ("cerberus" "13")
          ("amnesia" "9"))))
  (set-frame-font (concat "Cascadia Mono-" font-size) nil t))

;; Package setup

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; UI

(column-number-mode t)
(global-auto-revert-mode t)
(global-goto-address-mode t)
(global-hl-line-mode 1)
(recentf-mode t)
(repeat-mode t)
(savehist-mode t)
(set-fringe-mode 12)
(show-paren-mode 1)
(windmove-default-keybindings)
(winner-mode t)

(setq eldoc-echo-area-prefer-doc-buffer t)
(setq help-window-select t)
(setq inhibit-startup-message t)
(setq initial-buffer-choice 'recover-session)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq recentf-max-saved-items 200)
(setq shell-command-prompt-show-cwd t)
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)
(setq tab-bar-show 1)
(setq use-short-answers t)
(setq windmove-allow-all-windows t)

(setq display-buffer-alist
      `(("^\\(*help\\|*eldoc\\|*info\\|*Messages\\|*Warnings\\|COMMIT_EDITMSG\\)"
         (display-buffer-in-side-window)
         (side . left)
         (window-width . 80))
        ((or . ((derived-mode . comint-mode)
                (derived-mode . term-mode)
                "shell\\*$"
                "^\\*trace"))
         (display-buffer-in-side-window)
         (window-height . 0.5)
         (side . top))
        ((derived-mode . dape-info-parent-mode)
         (display-buffer-in-side-window)
         (side . right))))
(defvar my-display-buffer-alist display-buffer-alist)

(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-in-previous-window
         display-buffer-same-window
         display-buffer-use-some-window)))

(use-package avy
  :bind
  (("C-;" . avy-goto-line)
   ("C-'" . avy-goto-char-2)
   ("M-g g" . avy-goto-line)
   ("M-g M-g" . avy-goto-line)))

(use-package consult
  :bind
  (([remap Info-search] . consult-info)
   ("C-c h" . consult-history)
   ("C-x M-:" . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)
   ("C-M-#" . consult-register)
   ("M-y" . consult-yank-pop)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   :map comint-mode-map
   ("M-r" . consult-history)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
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
  ;; Allow using minibuffer when in minibuffer
  (setq enable-recursive-minibuffers t)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  :config
  (defun my-consult-buffer-display (buffer &optional norecord)
    (pop-to-buffer buffer
                   '((display-buffer-reuse-window
                      display-buffer-in-previous-window
                      display-buffer-same-window
                      display-buffer-use-some-window
                      display-buffer-pop-up-window))
                   norecord))
  (defun consult-buffer-pop-to-buffer (arg)
    (interactive "P")
    (let ((consult--buffer-display (if arg #'switch-to-buffer #'my-consult-buffer-display)))
      (consult-buffer)))
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

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))

  (setq corfu-map
        (define-keymap
          "<remap> <completion-at-point>" #'corfu-complete
          ;; XXX [tab] is bound because of org-mode and orgtbl-mode.
          ;; The binding should be removed from org-mode-map.
          "<tab>" #'corfu-complete
          "M-n" #'corfu-next
          "M-p" #'corfu-previous
          "C-g" #'corfu-quit
          "TAB" #'corfu-complete
          "M-SPC" #'corfu-move-to-minibuffer))
  (global-corfu-mode)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package diminish)

(use-package ef-themes
  :config
  (load-theme 'ef-duo-dark t))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package mode-line-bell
  :config
  (mode-line-bell-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package vertico
  :demand
  :bind
  (:map vertico-map
        ("RET"   . vertico-directory-enter)
        ("DEL"   . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  (setq vertico-count (/ (window-total-height) 3)))

(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(defun display-selected-buffer-other-window ()
  (interactive)
  (display-buffer (window-buffer) t))

(defun adjust-indenting ()
  "Adjust indenting based on major mode"
  (interactive)
  (cond
   ((derived-mode-p 'typescript-ts-mode)
    (setq-local typescript-ts-mode-indent-offset 4))
   ((derived-mode-p 'js-ts-mode)
    (setq-local js-indent-level 2))
   (t (message "Can't adjust indent in this major mode"))))

(bind-keys*
 ("C-<tab>" . winner-undo)
 ("C-S-<tab>" . winner-redo)
 ("C-<iso-lefttab>" . winner-redo)
 ("C-x k" . kill-current-buffer)
 ("C-x !" . delete-other-windows-vertically)
 ("C-x C-b" . ibuffer)
 ("C-c i" . adjust-indenting)
 ("C-c o" . display-selected-buffer-other-window)
 ("C-c q" . quit-window)
 ("C-." . eglot-code-action-quickfix))

;; Emacs behavior

(setq auto-save-timeout 5)
(setq bookmark-save-flag 1)
(setq completion-ignore-case t)
(setq custom-file (make-temp-file "emacs-custom"))
(setq interpreter-mode-alist nil)
(setq kill-buffer-delete-auto-save-files t)
(setq make-backup-files nil)
(setq vc-follow-symlinks t)

(with-eval-after-load 'project
  ;; TODO: still filter these projects based on gitignore
  (setq project-vc-extra-root-markers
	'("package.json"
	  "pyproject.toml"
	  "requirements.txt")))

;; Editing

(setq indent-line-function 'indent-basic-line)
(setq indent-region-function 'indent-basic-region)
(setq kill-whole-line 1)
(setq line-move-visual nil)
(setq tab-always-indent 'complete)

(add-to-list 'electric-indent-functions-without-reindent 'indent-basic)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default word-wrap t)

(electric-pair-mode t)

(defun indent-basic-line ()
  (cond
   ((not (eq this-command 'indent-for-tab-command))
    (indent-relative t))
   ((> (current-column) (current-indentation))
    nil)
   (t
    (back-to-indentation)
    (tab-to-tab-stop))))

(defun indent-basic-region (start end)
  (let (deactivate-mark)
    (indent-rigidly-right-to-tab-stop start end)))

(defun indent-region-no-deactivate (orig-fun &rest args)
  (if (eq indent-region-function 'indent-basic-region)
      (let (deactivate-mark)
        (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'indent-region :around #'indent-region-no-deactivate)

(defun indent-basic-left ()
  (interactive)
  (if (use-region-p)
      (let (deactivate-mark)
        (indent-rigidly-left-to-tab-stop (use-region-beginning)
                                         (use-region-end)))
    (indent-line-to (indent-next-tab-stop (current-indentation) t))))

(bind-keys
 ("<backtab>" . indent-basic-left))

;; Apps

(use-package magit
  :bind
  (:map magit-file-section-map
   ("RET" . magit-diff-visit-file-other-window)
   :map magit-hunk-section-map
   ("RET" . magit-diff-visit-file-other-window))
  :init
  (setq magit-diff-refine-hunk 'all)
  (setq magit-section-initial-visibility-alist
        '((stashes . hide)
	      (file . hide))))

(with-eval-after-load 'shell
  (add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
  (defun zsh-shell-mode-setup ()
    (setq-local comint-process-echoes t))
  (add-hook 'shell-mode-hook #'zsh-shell-mode-setup))

(with-eval-after-load 'compile
  (setq compilation-environment '("TERM=dumb"))
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

(with-eval-after-load 'dired
  (require 'dired-x)
  (setq dired-dwim-target t)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode))))

;; Programming

(with-eval-after-load 'prog-mode
  (defun prog-mode-config ()
    (setq-local show-trailing-whitespace t))
  (add-hook 'prog-mode-hook #'prog-mode-config))

;; TODO: c hungry delete and auto newline mode
;; c context line break and open line
;; maybe subword mode
;; activate c-mode for glsl files
(setq c-default-style "java")

(with-eval-after-load 'eglot
  (setq eglot-autoshutdown t)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-events-buffer-size 0)
  (setq eglot-extend-to-xref t)
  (setq eglot-ignored-server-capabilities
	'(:colorProvider
	  :inlayHintProvider))
  ;; TODO glsl_analyzer for c-mode glsl files
  (add-to-list 'eglot-server-programs
               '(glsl-ts-mode . ("glsl_analyzer"))))

(defun go-config ()
  (setq-local go-ts-mode-indent-offset 4)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  (defun my-eglot-organize-imports () (interactive)
	 (eglot-code-actions nil nil "source.organizeImports" t))
  (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
  (add-hook 'before-save-hook 'eglot-format-buffer nil t))
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'go-config)

(with-eval-after-load 'js
  (setq interpreter-mode-alist nil))

(use-package markdown-mode)

(with-eval-after-load 'python
  (setq interpreter-mode-alist nil))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs
	'(css
	  dockerfile
	  go
	  gomod
	  html
	  javascript
	  json
	  makefile
	  markdown
	  python
	  toml
	  tsx
	  typescript
	  yaml))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package squirrel-mode
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.nut\\'" . squirrel-mode))
  (defun squirrel-config ()
    (setq-local indent-tabs-mode t)
    (setq-local tab-width 4))
  (add-hook 'squirrel-mode-hook 'squirrel-config))

(use-package dape
  :init
  (setq dape-key-prefix "\C-cd")
  :config
  (remove-hook 'dape-on-start-hooks 'dape-info)
  (remove-hook 'dape-on-start-hooks 'dape-repl)
  (defun dape--save-on-start ()
    (save-some-buffers t t))
  (add-hook 'dape-on-start-hooks 'dape--save-on-start)
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (defun dape--fix-display-buffer (orig-fun &rest args)
    (let ((display-buffer-alist my-display-buffer-alist))
      (apply orig-fun args)))
  (advice-add 'dape--display-buffer :around #'dape--fix-display-buffer))

(defun web-mode-setup ()
  (setq-local electric-indent-inhibit t)
  (setq-local indent-region-function #'indent-region-line-by-line)
  (setq-local tab-width 2)
  (electric-pair-local-mode -1)
  (web-mode-set-engine "django"))
(use-package web-mode
  :hook html-mode
  :hook (web-mode . web-mode-setup)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-comment-indent-offset 2)
  (web-mode-markup-indent-offset 2))
