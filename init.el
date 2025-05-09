;; init.el --- Init file -*- lexical-binding: nil; -*-
;;; Commentary:
;;
;; ...
;;
;;; Code:

;; =============================================================================
;;                                   Elpaca
;; =============================================================================

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq elpaca-lock-file (expand-file-name "elpaca.lock" (file-name-directory load-file-name)))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(defun elpaca-gen-lockfile ()
  "Generate a new elpaca lock file to elpaca-lock-file"
  (interactive)
  (if (null elpaca-lock-file)
      (message "elpaca-lock-file == nil")
    (elpaca-write-lock-file elpaca-lock-file)))

;; =============================================================================
;;                                     Core
;; =============================================================================

(defmacro after! (files &rest body)
  "Eval BODY after loading all FILES."
  (if (null files)
      `(progn ,@body)
    `(with-eval-after-load ',(car files)
       (after! ,(cdr files) ,@body)))
  )

(defmacro add-hook! (files target)
  "Define hooks for all FILES to TARGET...
FILES should be an unquoted list.
TARGET should be a quoted mode"
  (cl-loop for val in files
           collect `(add-hook ',val ,target) into defs
           finally return `(progn ,@defs)))

;; =============================================================================
;;                                      UI
;; =============================================================================

(setq-default indent-tabs-mode nil tab-width 4)

(setq large-file-warning-threshold (* 1000 1000 50))

(pixel-scroll-precision-mode)

(setq base-font-height 140)
(set-face-attribute 'default nil
                    :family "Iosevka Pro")
(set-face-attribute 'variable-pitch nil
                    :family "Iosekva Pro")

(add-to-list 'default-frame-alist '(alpha-background . 90))

(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-message t)
(blink-cursor-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(global-visual-line-mode 1)


(setq whitespace-style '(face trailing)
      whitespace-global-modes '(not shell-mode
                                    help-mode
                                    vterm-mode
                                    magit-mode
                                    magit-diff-mode
                                    ibuffer-mode
                                    dired-mode
                                    occur-mode))

(global-whitespace-mode 1)

;; (use-package spacemacs-theme
;;   :ensure t
;;   :init
;;   (load-theme 'spacemacs-dark t))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; (doom-themes-visual-bell-config)
  ;; (doom-themes-neotree-config)
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-time-clock-size 3.0
        doom-modeline-height 5)
  (display-time-mode 1)
  (column-number-mode 1))

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO" success bold)
          ("FIXME" error bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold)
          ("BUG" error bold)))
  (global-hl-todo-mode 1)
  )

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(setq title-list '("Coping with radical novelty requires an orthogonal method. One must consider one's own past, the experiences collected, and the habits formed in it as an unfortunate accident of history, and one has to approach the radical novelty with a blank mind, consciously refusing to try to link it with what is already familiar, because the familiar is hopelessly inadequate. One has, with initially a kind of split personality, to come to grips with a radical novelty as a dissociated topic in its own right. Coming to grips with a radical novelty amounts to creating and learning a new foreign language that can not be translated into one's mother tongue. (Any one who has learned quantum mechanics knows what I am talking about.) Needless to say, adjusting to radical novelties is not a very popular activity, for it requires hard work. For the same reason, the radical novelties themselves are unwelcome. - Dijkstra"
                   "The lesson I learned is that you can wildly, drastically improve brain interconnectivity and neuroplasticity by being obsessed with practicing a skill that you’re absolutely terrible at, for two months. - Tater Tot"))

;; TODO add a hook to reopen the buffer if it doesn't exit when another is deleted
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title
        (nth (random (length title-list)) title-list))
  (setq dashboard-startup-banner
        (let* ((img-file (expand-file-name "~/Sync/emacs.jpg"))
               (txt-file (expand-file-name "banners/looking.txt" user-emacs-directory))
               (img-exists (file-exists-p img-file))
               (txt-exists (file-exists-p txt-file)))
          (cond
           ((and img-exists txt-exists) (cons img-file txt-file))
           (img-exists (cons img-file 3))
           (txt-exists (cons 'logo txt-file))
           (t (cons 'logo 3)))))

  (setq dashboard-items '((recents   . 10)
                          ;; (bookmarks . 5)
                          (projects  . 5)
                          ;; (registers . 5)
                          (agenda    . 5)))
  ;; NOTE: goal here is that if we open emacs via cli and pass it file args, that we won't
  ;; end up with the dashboard in one window
  (if (>= 1 (length command-line-args)) (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))
  (dashboard-setup-startup-hook))

;; =============================================================================
;;                                     Evil
;; =============================================================================

(use-package evil
  :ensure t
  :after undo-tree
  :init
  (setq evil-undo-system 'undo-tree
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-goggles
  :ensure t
  :after evil
  :init
  (add-hook 'evil-mode-hook #'evil-goggles-mode)
  :config
  (evil-goggles-mode))

(use-package evil-snipe
  :ensure t
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  :custom
  (evil-snipe-scope 'whole-visible) ; Search in whole buffer instead of just line
  (evil-snipe-repeat-scope 'whole-visible) ; Same for repeat
  (evil-snipe-smart-case t)         ; Smart case sensitivity
  )

(use-package undo-tree
  :ensure t
  :after no-littering
  :init
  (global-undo-tree-mode 1)
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "undo-tree-hist/")))))

;; =============================================================================
;;                                   Completion
;; =============================================================================

(use-package vertico
  :ensure t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode)
  :config
  (after! (general)
          (general-setq enable-recursive-minibuffers t)
          (minibuffer-depth-indicate-mode)
          (general-setq minibuffer-prompt-properties
                        '(read-only t face minibuffer-prompt intangible t cursor-intangible t))
          (general-add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)))

(use-package vertico-directory
  :ensure nil
  :after vertico general
  :init
  (add-hook 'rfn-esm-update-handlers #'vertico-directory-tidy)
  :config
  (general-def
    :keymaps 'vertico-map
    "RET" 'vertico-directory-enter
    ;; this means backspace goes down a whole dir, i'm not really use why <backspace> gets
    ;; mapped to DEL in that the minibuffer, but it does
    "DEL" 'vertico-directory-delete-char
    "M-DEL" 'vertico-directory-delete-word))

(use-package consult
  :ensure t
  :after vertico
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (defvar dynamic-diagnostic-fn #'consult-flymake
    "Consult diagnostics dyn fn, changes between consult-flymake and consult-flycheck based on mode")
  (add-hook 'flymake-mode-hook (cmd! (setq dynamic-diagnostic-fn #'consult-flymake)))
  (add-hook 'flycheck-mode-hook (cmd! (setq dynamic-diagnostic-fn #'consult-flycheck)))

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.1 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.3 any))

  (setq consult-narrow-key "<")

  (after! (evil)
          (setq evil-jumps-cross-buffers nil)
          (evil-set-command-property 'consult-line :jump t)))

(use-package consult-flycheck
  :ensure t
  :after consult flycheck)

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode)
  :custom
  (marginalia-align 'right)
  (marginalia-max-relative-age 0)
  :config
  ;; Add custom annotators for more informative completions
  (add-to-list 'marginalia-prompt-categories '("\\<face\\>" . face))
  (add-to-list 'marginalia-prompt-categories '("\\<var\\>" . variable)))


(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (global-corfu-mode) ;; Enable Corfu globally

  :config
  (setq corfu-auto      t
        corfu-auto-delay  0.2
        corfu-auto-prefix 2)

  ;; (add-hook 'corfu-mode-hook
  ;;           (lambda ()
  ;;             ;; Settings only for Corfu
  ;;             (setq-local completion-styles '(basic)
  ;;                         completion-category-overrides nil
  ;;                         completion-category-defaults nil)))
  )

;; TODO remove this
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (partial-completion basic)))
                                   (project-file (styles . (partial-completion basic)))))
  :config
  ;; Recognize more completion styles
  (setq orderless-matching-styles
        '(orderless-literal
          orderless-prefixes
          orderless-initialism
          orderless-regexp)))

;; =============================================================================
;;                                  Programming
;; =============================================================================

(electric-pair-mode 1)

(setq electric-pair-pairs '())
(defun my-electric-pair-specific-mode-config ()
  (cond
   ((member major-mode '(rust-mode))
    (setq-local electric-pair-pairs '((?\" . ?\")
                                      (?\' . ?\')
                                      (?\{ . ?\})
                                      (?\( . ?\))
                                      (?\[ . ?\])))))
  )
(add-hook 'after-change-major-mode-hook 'my-electric-pair-specific-mode-config)

;; TODO: setup formatter for elisp and rust
(use-package apheleia
  :ensure t
  :config
  (add-hook! (nix-mode-hook rust-mode-hook emacs-lisp-mode-hook) #'apheleia-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook #'rust-mode-hook #'flycheck-mode))

(use-package tree-sitter-langs
  :ensure t)

(use-package tree-sitter
  :after tree-sitter-langs
  :ensure t
  :init
  (tree-sitter-require 'nix)
  (tree-sitter-require 'rust)
  :config
  ;; (tree-sitter-hl-mode)
  ;; (add-hook 'rust-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (global-tree-sitter-mode))

;; =============================================================================
;;                                  Languages
;; =============================================================================

(use-package rust-mode
  :ensure t
  :config
  (after! (eglot)
          (add-hook 'rust-mode-hook 'eglot-ensure)
          (add-to-list 'eglot-server-programs
                       '(rust-mode . ("rust-analyzer"))))
  (after! (apheleia)
          (setf (alist-get 'rust-mode apheleia-mode-alist) 'rustfmt)
          (setf (alist-get 'rustfmt apheleia-formatters) '("rustfmt" "--edition" "2021")))
  (after! (tree-sitter-langs)
          (setq rust-mode-treesitter-derive t))
  (setq rust-cargo-default-arguments "--color=never"))

;; (use-package rustic
;;   :ensure t
;;   :mode ("\\.rs\\'" . rust-mode)
;;   :mode ("\\.rs\\'" . rustic-mode)
;;   :config
;;   (setq rustic-format-on-save nil)
;;   :custom
;;   (rustic-cargo-use-last-stored-arguments t))

;; (use-package cargo
;;   :ensure t
;;   :config
;;   (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package nix-ts-mode
  :ensure t
  :custom
  (nix-nixfmt-bin "nixfmt")
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
  (after! (apheleia)
          (setf (alist-get 'nix-ts-mode apheleia-mode-alist) 'nixfmt)
          (setf (alist-get 'nixfmt apheleia-formatters)
                '("nixfmt" "-"))))

;; =============================================================================
;;                                    LSP
;; =============================================================================

;; NOTE: conflicts with lsp-mode
;; (require 'eglot)
;; (setq eglot-autoshutdown t
;;       eglot-events-buffer-size 0
;;       eglot-sync-connect nil
;;       eglot-connect-timeout 300
;;       eglot-auto-display-help-buffer nil)

;; NOTE: conflicts with eglot
(use-package lsp-mode
  :ensure t
  :custom
  (lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (lsp-rust-analyzer-proc-macro-enable t)
  :config
  ;; lsp-deferred is supposed to help with catching the right envrc env, but it
  ;; doesn't seem to
  (add-hook! (nix-mode-hook rust-mode-hook) #'lsp-deferred))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-doc-side 'right)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-enable)
  :config
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-max-width 150
        lsp-ui-doc-max-height 24))

;; =============================================================================
;;                               Project Management
;; =============================================================================

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :custom
  (projectile-enable-caching t)
  (projectile-globally-ignored-directories
   '(".git" ".log" "build" "node_modules" ".direnv" "target"))
  :config
  (add-to-list 'projectile-project-root-files "shell.nix")
  (add-to-list 'projectile-project-root-files "flake.nix"))

(use-package rg
  :ensure t)

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package envrc
  :ensure t
  :after (lsp-mode flycheck)
  :config
  (envrc-global-mode))

;; =============================================================================
;;                                    Tools
;; =============================================================================

(use-package transient :ensure t) ;; Dep of Magit
(use-package magit
  :after transient
  :ensure t)

(use-package vterm
  :ensure nil
  :commands vterm
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm: %s")
  :config
  (evil-set-initial-state 'vterm-mode 'insert))

(use-package ranger
  :ensure t
  :config
  (setq ranger-show-hidden t
        ranger-preview-delay 0.1
        ranger-width-parents 0.16
        ranger-width-preview 0.5
        ranger-max-preview-size 10))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 16)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package w3m
  :ensure t)

;; =============================================================================
;;                                   Org Mode
;; =============================================================================

(use-package org
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (setq-local fill-column 100)))
  (add-hook 'org-mode-hook #'org-indent-mode)
  (unless (file-exists-p "~/Sync/org")
    (make-directory "~/Sync/org" t))
  (setq org-blank-before-new-entry
        '((heading . nil)    ;; No blank line before new headings
          (plain-list-item . nil)))  ;; No blank line before new items
  (setq org-directory "~/Sync/org"
        org-agenda-files '("~/Sync/org/agenda.org")))

(use-package evil-org
  :ensure t
  :after evil org
  :custom
  (evil-org-key-theme '(additional
                        calendar
                        heading
                        insert
                        navigation
                        return
                        shift
                        textobjects
                        todo))

  :config
  (add-hook 'org-mode-hook #'evil-org-mode)
  (evil-org-set-key-theme)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-superstar
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook #'org-superstar-mode))

(use-package org-appear
  :ensure t
  :after org
  :custom
  (org-appear-autolinks t)
  (org-appear-delay 1)
  :config
  (add-hook 'org-mode-hook #'org-appear-mode))

;; =============================================================================
;;                                 Odds and Ends
;; =============================================================================

(use-package recentf
  :ensure nil
  :after no-littering
  :custom
  (recentf-max-saved-items 512)
  :init
  (recentf-mode 1)
  (load-file recentf-save-file)
  :config
  (add-hook 'kill-emacs-hook 'recentf-save-list))

(setq create-lockfiles nil)

;; Custom file
;; TODO move to emacs config dir?
(setq custom-file (expand-file-name "custom.el" "~/.local/state/emacs/"))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/"))))

  ;; Keep transient files organized
  (setq transient-history-file (no-littering-expand-var-file-name "transient/history.el")
        transient-levels-file (no-littering-expand-etc-file-name "transient/levels.el")
        transient-values-file (no-littering-expand-etc-file-name "transient/values.el")))

(setq gc-cons-threshold (* 96 1024 1024))
(add-hook 'focus-out-hook #'garbage-collect)

;; =============================================================================
;;                                     Maps
;; =============================================================================

(use-package general
  :ensure t
  :config
  (general-evil-setup))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.2)
  (which-key-prefix-prefix "+")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  :config
  (add-hook 'which-key-init-buffer-hook
            (lambda ()
              (setq-local mode-line-format nil)))
  (setq which-key-show-mode-line nil
        which-key-allow-imprecise-window-fit nil)
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; Make sure which-key buffer is always below minibuffer
  (setq which-key-popup-type 'side-window))

(defun reload-config ()
  "Reload the current config's init.el."
  (interactive)
  (load-file (expand-file-name "lib.el" user-emacs-directory))
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(after! (general lib)
        ;; Global
        (map-leader! :n '(global evil-mode-map) "SPC"
                     ";" #'eval-expression
                     ":" #'eval-last-sexp
                     ;; Buffers
                     "b b" #'consult-buffer
                     "b d" '("Kill current buffer." . kill-current-buffer)
                     ;; Emacs
                     "e k" '("Kill Emacs" . save-buffers-kill-emacs)
                     "e r" '("Reload config!" . reload-config)
                     ;; Find
                     "f f" '("Open a file!" . find-file)
                     "f r" '("Open a recent file!" . consult-recent-file)
                     ;; Open
                     "o e" '("Open Eshell!" . eshell)
                     "o r" '("Open Ranger!" . ranger)
                     "o m" '("Open a MAGIT!" . magit)
                     "o t" '("Open Treemacs!" . treemacs)
                     "o v" '("Open vTerm!" . vterm)
                     ;; Search
                     "s w" '("Search: Wikipedia" . search-wikipedia)
                     ;; Window
                     "w h" #'evil-window-left
                     "w j" #'evil-window-down
                     "w k" #'evil-window-up
                     "w l" #'evil-window-right
                     "w s" #'evil-window-split
                     "w v" #'evil-window-vsplit)
        (map-leader-after! (evil) :n '(global evil-mode-map) "f"
                           "b" 'consult-buffer
                           "l" 'consult-line
                           "f" 'consult-fd
                           "r" 'consult-ripgrep
                           "m" 'consult-imenu
                           "o" 'consult-org-heading
                           "d" (cmd! (funcall dynamic-diagnostic-fn)))
        (map-after! (evil) :nv '(global evil-mode-map)
                    "t" #'comment-line)
        (map-after! (evil) :n '(global evil-mode-map)
                    "9" (cmd! (scroll-up 18))
                    "0" (cmd! (scroll-down 18))
                    "C-=" (cmd! (set-face-attribute 'default nil :height base-font-height))
                    "C-+" #'increase-global-font-size
                    "C--" #'decrease-global-font-size
                    "M-j" #'evil-window-left
                    "M-k" #'evil-window-down
                    "M-l" #'evil-window-up
                    "M-;" #'evil-window-right
                    "U" #'evil-redo)
        ;; Modal
        (map-after! (evil-collection) :nv vterm-mode-map
                    "C-d" 'vterm--self-insert
                    ;; NOTE: evil-collection binds C-c C-z to evil-collection-vterm-toggle-send-escape
                    ;; normally, so this is a workaround for now
                    "C-c C-c" 'vterm--self-insert
                    "I" (cmd! (vterm-reset-cursor-point) (evil-insert 0)))
        (map-leader! :n rust-mode-map "SPC"
                     "b" #'rust-compile
                     "r" #'rust-run
                     "t" #'rust-test
                     "c" #'rust-check)
        (map-after! (lsp-ui) :n lsp-ui-mode-map
                    "." #'lsp-ui-doc-glance)
        (map! :n ranger-mode-map
              "DEL" #'ranger-toggle-dotfiles)
        (map! :n eww-mode-map
              "H" #'eww-back-url)
        (map-after! (org evil-org) :ni '(org-mode-map evil-org-mode-map)
                    ;; NOTE: These are for manipulating subtrees, it is beyond me why the naming
                    ;; convention is like this
                    "C-h" #'org-shiftmetaleft
                    "C-j" #'org-metadown
                    "C-k" #'org-metaup
                    "C-l" #'org-shiftmetaright)
        )

;; =============================================================================
;;                                     Footer
;; =============================================================================

(load-file (expand-file-name "lib.el" (file-name-directory load-file-name)))

(provide 'init)
;;; init.el ends here
