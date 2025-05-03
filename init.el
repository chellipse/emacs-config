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

;; (defmacro setq! (&rest pairs)
;;   "Define multiple variables.  PAIRS should be in the form (SYMBOL INITVALUE)..."
;;   (let ((triplets nil))
;;     (cl-loop for (var val) on pairs by #'cddr
;;              when (and var val)
;;              collect `(defvar ,var ,val) into defs
;;              finally return `(progn ,@defs))))

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

(set-face-attribute 'default nil
                    :family "Iosevka Pro"
                    :height 160)
(set-face-attribute 'variable-pitch nil
                    :family "Iosekva Pro")

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
  :init (doom-modeline-mode 1))

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

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t))

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
    "DEL" 'vertico-directory-delete-char
    "M-DEL" 'vertico-directory-delete-word))

(use-package vertico-multiform
  :ensure nil
  :after vertico
  :custom
  (vertico-buffer-display-action '(display-buffer-use-least-recent-window))
  (vertico-multiform-categories
   '((execute-extended-command grid)
     (library reverse grid)
     (consult-buffer vertical)
     (consult-grep reverse grid)))

  :config
  (vertico-multiform-mode))

(use-package vertico-repeat
  :ensure nil
  :after vertico
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (general-def
    :states '(normal insert visual motion)
    "C-M-;" 'vertico-repeat))

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

;; TODO: setup formatter for elisp and rust
(use-package apheleia
  :ensure t
  :config
  (add-hook! (nix-mode-hook rust-mode-hook emacs-lisp-mode-hook) #'apheleia-mode))

;; (use-package flycheck
;;   :ensure t
;;   ;; :config
;;   ;; (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
;;   )

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
  (after! (lsp-mode)
	  (add-hook 'rust-mode-hook #'lsp))
  (after! (apheleia)
	  (setf (alist-get 'rust-mode apheleia-mode-alist) 'rustfmt)
	  (setf (alist-get 'rustfmt apheleia-formatters) '("rustfmt" "--edition" "2021")))
  (after! (tree-sitter-langs)
	  (setq rust-mode-treesitter-derive t))
  (setq rust-cargo-default-arguments "--color=never")
  ;; (my-local-leader-def
  ;; :keymaps 'rust-mode-map
  ;; "b" 'rust-compile
  ;; "r" 'rust-run
  ;; "t" 'rust-test
  ;; "c" 'rust-check)
  )

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

;; ;; =============================================================================
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
  :config
  (add-hook! (nix-mode-hook rust-mode-hook) #'lsp-mode))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-doc-side 'right)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-enable)
  :config
  (setq lsp-ui-doc-max-width 150
        lsp-ui-doc-max-height 24))

;; =============================================================================
;;                               Project Management
;; =============================================================================

(use-package transient :ensure t) ;; Dep of Magit

(use-package magit
  :after transient
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :custom
  (projectile-enable-caching t)
  (projectile-globally-ignored-directories
   '(".git" ".log" "tmp" "dist" "*node_modules" ".direnv" "*target" "*.lsp" "*.clj-kondo"))
  :config
  (add-to-list 'projectile-project-root-files "shell.nix")
  (add-to-list 'projectile-project-root-files "flake.nix"))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

;; =============================================================================
;;                                   Org Mode
;; =============================================================================

(use-package org
  :ensure t
  :config
  (unless (file-exists-p "~/Sync/org")
    (make-directory "~/Sync/org" t))
  (setq org-directory "~/Sync/org"
        org-agenda-files '("~/Sync/org/agenda.org")))

(use-package evil-org
  :ensure t
  :after evil org
  :hook (org-mode-hook . evil-org-mode)
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
  (evil-org-set-key-theme)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; =============================================================================
;;                                 Odds and Ends
;; =============================================================================

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 512)
  :init
  (recentf-mode 1))

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

(defvar my/gc-cons-threshold (* 96 1024 1024))
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
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; Make sure which-key buffer is always below minibuffer
  (setq which-key-popup-type 'side-window))

;; TODO: setup leader key
(after! (evil general lib)
	;; Leader
	;; Global Maps
	(map! :nv global
	      "t" #'comment-line)
	(map! :n global
	      "9" (cmd! (scroll-up 18))
	      "0" (cmd! (scroll-down 18))
	      "L" #'evil-next-buffer
	      "K" #'evil-prev-buffer
	      "C-;" #'eval-expression
	      "U" #'evil-redo)
	;; Mode Maps
	(map! :n eww-mode-map
	      "H" #'eww-back-url)
	(after! (lsp-ui)
		(map! :n lsp-ui-mode-map
		      "." #'lsp-ui-doc-glance))
	)

;; =============================================================================
;;                                     Footer
;; =============================================================================

(load-file (expand-file-name "lib.el" (file-name-directory load-file-name)))

(provide 'init)
;;; init.el ends here
