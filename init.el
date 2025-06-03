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

(use-package general :ensure (:wait t) :demand t
  :config
  (general-evil-setup))

(defmacro load! (file)
  "Load user-emacs-directory/FILE."
  `(load-file (concat (expand-file-name user-emacs-directory) ,file)))

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

;; (setq default-font-mono "Iosevka Pro"
;;       default-font-propo "Iosevka Nerd Font Propo")
(setq default-font-mono "RobotoMono Nerd Font"
      default-font-propo "Roboto")

(setq default-font-size 14)
(unless (boundp 'font-size)
  (setq font-size default-font-size))
(set-face-attribute 'default nil
                    :height (* font-size 10)
                    :family default-font-mono)
(set-face-attribute 'variable-pitch nil
                    :family default-font-propo)

(load-theme 'modus-operandi)

;; (load! "modules/ui/doom.el")
(load! "modules/ui/doom-modeline.el")
;; (load! "modules/ui/spacemacs.el")
;; (load! "modules/ui/nano.el")

(setq-default indent-tabs-mode nil tab-width 4)

(setq large-file-warning-threshold (* 1000 1000 50))

(pixel-scroll-precision-mode)
(setq scroll-conservatively 1000
      scroll-preserve-screen-position 'always)

(global-hl-line-mode 1)

(setq default-frame-alpha 100)
(add-to-list 'default-frame-alist `(alpha-background . ,default-frame-alpha))

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


;; TODO add a hook to reopen the buffer if it doesn't exit when another is deleted
(use-package dashboard
  :ensure t
  :config
  ;; (defvar title-list nil)
  ;; (setq dashboard-banner-logo-title
  ;;       (nth (random (length title-list)) title-list))
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

(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 120))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

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
        evil-flash-delay 10 ;; Search flash after n/N. Default: 2 (seconds)
        evil-want-C-u-scroll t
        evil-want-C-i-jump t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-want-unimpaired-p nil)
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
  (vertico-count 15) ;; Show more candidates
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

(use-package rainbow-mode
  :ensure t)

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
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt) ;; Always preselect the prompt

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)

  :config
  (setq corfu-auto      t
        corfu-auto-delay  0.2
        corfu-auto-prefix 2
        corfu-popupinfo-delay 0.5)

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

(use-package cape
  :ensure t
  :hook (org-mode . (lambda ()
                      (add-to-list 'completion-at-point-functions
                                   #'cape-file))))

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
  :after (general lib)
  :config
  (defun rust-current-crate-name ()
    "Get the name of the current crate by parsing Cargo.toml in the crate directory."
    (let ((crate-dir (rust-buffer-crate)))
      (when crate-dir
        (let ((cargo-toml (expand-file-name "Cargo.toml" crate-dir)))
          (when (file-exists-p cargo-toml)
            (with-temp-buffer
              (insert-file-contents cargo-toml)
              (goto-char (point-min))
              (when (re-search-forward "^name\\s-*=\\s-*\"\\([^\"]+\\)\"" nil t)
                (match-string 1))))))))

  (defmacro cargo-cmd! (cmd &key scope)
    "Generate a lambda that calls rust--compile with CMD as the cargo subcommand.
SCOPE can be:
  'workspace - adds --workspace flag
  'package   - adds --package with current crate name
  anything else or nil - uses raw command with no package/workspace flags"
    `(lambda ()
       (interactive)
       (cond
        ((eq ,scope 'workspace)
         (rust--compile nil
                        ,(concat "%s " cmd " --workspace %s")
                        rust-cargo-bin
                        rust-cargo-default-arguments))
        ((eq ,scope 'package)
         (let ((crate-name (rust-current-crate-name)))
           (if crate-name
               (rust--compile nil
                              ,(concat "%s " cmd " --package %s %s")
                              rust-cargo-bin
                              crate-name
                              rust-cargo-default-arguments)
             (rust--compile nil
                            ,(concat "%s " cmd " %s")
                            rust-cargo-bin
                            rust-cargo-default-arguments))))
        (t
         (rust--compile nil
                        ,(concat "%s " cmd " %s")
                        rust-cargo-bin
                        rust-cargo-default-arguments)))))

  (map-leader! :n rust-mode-map "SPC"
               "c x" #'lsp-rust-analyzer-run
               "c r" (cargo-cmd! "run" :scope 'package)
               "c R" (cargo-cmd! "run --release" :scope 'package)
               "c b" (cargo-cmd! "build" :scope 'workspace)
               "c B" (cargo-cmd! "build --release" :scope 'workspace)
               "c t" (cargo-cmd! "test" :scope 'package)
               "c T" (cargo-cmd! "test" :scope 'workspace)
               "c c" (cargo-cmd! "check" :scope 'package)
               "c C" (cargo-cmd! "check" :scope 'workspace))

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
  (setq lsp-completion-provider :none
        lsp-lens-enable nil
        lsp-auto-configure t)
  ;; lsp-deferred is supposed to help with catching the right envrc env, but it
  ;; doesn't seem to
  (add-hook! (nix-mode-hook rust-mode-hook c-mode-hook) #'lsp-deferred))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-doc-side 'right)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-enable)
  :config
  (set-face-attribute 'lsp-ui-doc-background nil
                      :background nil)
  (add-hook 'server-after-make-frame-hook (lambda () (set-face-attribute 'lsp-ui-doc-background nil
                                                                         :background nil)))
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-max-width 150
        lsp-ui-doc-max-height 40))

;; Used for lsp-mode code actions and stuff
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

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
  :ensure t
  :config
  (add-hook 'w3m-mode-hook (lambda () (setq-local display-line-numbers nil))))

(use-package gptel
  :ensure t
  :config
  (setq gptel-default-mode 'org-mode
        gptel-track-media t
        gptel-org-branching-context t
        gptel-expert-commands t
        gptel-directives '((default
                            . "You are a large language model living in Emacs and a helpful assistant. Repond using these key communication principles.

Key Communication Principles:
* Strictly avoid using analogies; instead, employ precise technical language
* Avoid sycophantic language or appeals to the reader, prioritize logic
* Structure information like a scientific paper, ie with rigorous logical structure
* Correctness comes before pleasantry
* Maintain scholarly objectivity
* Communicate through direct, unambiguous technical language that eliminates interpretative ambiguity.")
                           (programming
                            . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
                           (writing
                            . "You are a large language model and a writing assistant. Respond concisely.")
                           (chat
                            . "You are a large language model and a conversation partner. Respond concisely."))
        gptel-model 'mistral-nemo:latest
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models
                        '(qwen3:0.6b
                          qwen3:1.7b
                          qwen3:4b
                          qwen3:8b
                          qwen3:14b
                          qwen2.5:14b
                          mistral-nemo:latest
                          mistral-small:22b
                          mistral-small:22b-instruct-2409-q3_K_S
                          mistral-small:22b-instruct-2409-q3_K_L
                          mistral-small:22b-instruct-2409-q3_K_M
                          (mistral-small3.1:latest . (
                                                      :capabilities (media)
                                                      :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")))
                          (gemma3:4b . (
                                        :capabilities (media)
                                        :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")))
                          (gemma3:12b . (
                                         :capabilities (media)
                                         :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")))
                          deepseek-r1:1.5b
                          deepseek-r1:14b
                          phi4-mini:latest
                          phi4:latest
                          deepseek-r1:8b
                          mistral:instruct
                          mistral:text
                          mistral:latest
                          llama3.2:latest))))


(use-package elfeed
  :ensure t
  :config
  (setq elfeed-search-filter "@2-week-ago"
        ;; elfeed-show-entry-switch #'pop-to-buffer
        elfeed-feeds '(
                       ("https://loginasroot.net/rss.xml" friend)
                       ("https://alicemaz.substack.com/feed" friend software)
                       ("https://faroffunhappythings.com/?feed=rss2" FOUT)
                       ("https://www.kenklippenstein.com/feed" journalism)
                       ("https://xkcd.com/atom.xml" comic)
                       ("https://fasterthanli.me/index.xml" rust)
                       ("https://lwn.net/headlines/rss" linux oss)
                       ("https://cafkafk.dev/index.xml" software)
                       ("https://www.lineageos.org/feed-engineering.xml" android)
                       ("https://research.google/blog/rss/" google)
                       ("https://deepmind.google/blog/rss.xml" google ml)
                       ("https://terrytao.wordpress.com/feed/" math)
                       ("https://karthinks.com/software/index.xml" emacs)
                       ("https://parakeet.substack.com/feed" tpot)
                       ;; ("")

                       ("https://thahxa.tumblr.com/rss" friend tumblr)
                       ("https://centrally-unplanned.tumblr.com/rss" tumblr)
                       ("https://vren-diagram.tumblr.com/rss" tumblr)
                       ("https://phaeton-flier.tumblr.com/rss" tumblr)
                       ("https://transgenderer.tumblr.com/rss" tumblr)
                       ("https://wildgifthorses.tumblr.com/rss" tumblr)
                       ("https://autogeneity.tumblr.com/rss" tumblr)

                       ("https://www.helius.dev/blog/rss.xml" solana)
                       ("https://apfitzge.github.io/index.xml#feed" solana)

                       ("https://danluu.com/atom.xml" programming industry)
                       ("https://www.kalzumeus.com/feed/articles/" finance industry)
                       ("https://www.thediff.co/archive/rss/" finance)
                       ("https://rss.beehiiv.com/feeds/JyXsSUwlAE.xml" finance)
                       ("https://www.bloomberg.com/authors/ARbTQlRLRjE/matthew-s-levine.rss" finance)
                       ("https://www.bitsaboutmoney.com/archive/rss/" finance industry)
                       ("https://feeds.transistor.fm/complex-systems-with-patrick-mckenzie-patio11" finance industry)
                       ;; ("")
                       )))

;; =============================================================================
;;                                   Org Mode
;; =============================================================================

(use-package org
  :ensure t
  :config
  (setq org-preview-latex-image-directory "/tmp/ltximg/"
        org-preview-latex-default-process 'luadvisvgm)
  (add-to-list 'org-preview-latex-process-alist
               '(luadvisvgm :programs
                            ("lualatex" "dvisvgm")
                            :description "dvi > svg" :message "you need to install the programs: lualatex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
                            (1.7 . 1.5)
                            :latex-compiler
                            ("lualatex --output-format dvi --shell-escape --interaction=nonstopmode --output-directory=%o %f")
                            :image-converter
                            ("dvisvgm %f -n -b min -c %S -o %O"))
               )


  (defun my-org-latex-preview-at-point ()
    "Preview LaTeX fragment at point when using C-c C-c."
    (when (org-inside-LaTeX-fragment-p)
      (org-latex-preview)
      t))  ; Return t to prevent other C-c C-c actions
  (add-hook 'org-ctrl-c-ctrl-c-hook #'my-org-latex-preview-at-point)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)))

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  (setq org-confirm-babel-evaluate
        (lambda (lang body)
          (not (member lang '("gnuplot" "rust")))))

  (defun my-gnuplot-unique-filename ()
    "Generate unique filename for gnuplot output."
    (concat "/tmp/plot-" (format-time-string "%s-%N") ".png"))

  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.25))

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

(use-package gnuplot
  :ensure t
  :after org)

(use-package org-fragtog
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook #'org-fragtog-mode))

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
                     "a" #'lsp-execute-code-action
                     "k" #'lsp-ui-doc-glance
                     "r" #'lsp-rename
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
                     "o g" #'gptel
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
                     "w v" #'evil-window-vsplit
                     ;; Zen
                     "z i" #'writeroom-increase-width
                     "z d" #'writeroom-decrease-width
                     "z z" #'writeroom-mode)
        (map-leader-after! (evil) :n '(global evil-mode-map) "f"
                           "f" 'consult-buffer
                           "l" 'consult-line
                           "d" 'consult-fd
                           "r" 'consult-ripgrep
                           "m" 'consult-imenu
                           "o" 'consult-org-heading
                           "d" (cmd! (funcall dynamic-diagnostic-fn)))
        (map-after! (evil) :nv '(global evil-mode-map)
                    ;; "d" #'evil-delete-char
                    "g e" #'evil-goto-line
                    "t" #'comment-line)
        (map-after! (evil) :v '(global evil-mode-map)
                    ;; "x" #'evil-next-line
                    )
        (map-after! (evil) :n '(global evil-mode-map)
                    ;; "x" #'evil-visual-line
                    "9" (cmd! (scroll-up 18))
                    "0" (cmd! (scroll-down 18))
                    "M-+" (cmd! (set-frame-parameter nil 'alpha-background default-frame-alpha))
                    "M-=" (cmd! (set-frame-parameter nil 'alpha-background (+ (frame-parameter nil 'alpha-background) 1)))
                    "M--" (cmd! (set-frame-parameter nil 'alpha-background (- (frame-parameter nil 'alpha-background) 1)))
                    "C-+" (cmd! (setq font-size default-font-size) (refresh-global-font-size))
                    "C-=" #'increase-global-font-size
                    "C--" #'decrease-global-font-size
                    "M-j" #'evil-window-left
                    "M-k" #'evil-window-down
                    "M-l" #'evil-window-up
                    "M-;" #'evil-window-right
                    "U" #'evil-redo)
        ;; Modal
        (map-after! (vterm evil-collection) :n vterm-mode-map
                    "C-d" 'vterm--self-insert
                    ;; NOTE: evil-collection binds C-c C-z to evil-collection-vterm-toggle-send-escape
                    ;; normally, so this is a workaround for now
                    "C-c C-c" 'vterm--self-insert
                    "I" (cmd! (vterm-reset-cursor-point) (evil-insert 0)))
        (map-after! (lsp-mode lsp-ui) :n lsp-ui-mode-map
                    "g y" #'lsp-goto-type-definition
                    "g i" #'lsp-goto-implementation
                    "g d" #'evil-goto-definition
                    "," #'lsp-ui-doc-toggle
                    "." #'lsp-ui-doc-glance)
        (map! :n ranger-mode-map
              "DEL" #'ranger-toggle-dotfiles)
        (map! :n eww-mode-map
              "H" #'eww-back-url)
        ;; FIXME: these things break certain binds since org-mode binds depend on what element the cursor is over
        ;; so we need to figure something better out if these are gonna work without messing things up
        ;; (map-after! (org evil-org) :ni '(org-mode-map evil-org-mode-map)
        ;;             ;; NOTE: These are for manipulating subtrees, it is beyond me why the naming
        ;;             ;; convention is like this
        ;;             "C-h" #'org-shiftmetaleft
        ;;             "C-j" #'org-metadown
        ;;             "C-k" #'org-metaup
        ;;             "C-l" #'org-shiftmetaright)
        (map! :n elfeed-show-mode-map
              "<up>" 'elfeed-show-prev
              "<down>" 'elfeed-show-next)
        )

;; =============================================================================
;;                                     Footer
;; =============================================================================

(load! "lib.el")

(elpaca-process-queues)

(provide 'init)
;;; init.el ends here
