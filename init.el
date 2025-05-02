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

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; =============================================================================
;;                                     UI
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
  :hook
  (prog-mode . rainbow-delimiters-mode))


;; =============================================================================
;;                                     Evil
;; =============================================================================

(use-package evil
  :ensure t
					; :after (undo-tree)
					; :init
					; (setq evil-want-integration t
					; evil-want-keybinding nil
					; evil-want-C-u-scroll t
					; evil-want-C-i-jump t
					; evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

(use-package evil-goggles
  :ensure t
  :after evil
  :hook evil-mode
  :config
  (evil-goggles-mode))

;; TODO set evil undo system
(use-package evil-snipe
  :ensure t
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  :custom
  (evil-snipe-scope 'buffer)        ; Search in whole buffer instead of just line
  (evil-snipe-repeat-scope 'buffer) ; Same for repeat
  (evil-snipe-smart-case t)         ; Smart case sensitivity
  )

;;(use-package undo-tree
;;  :ensure t
;;  :init
;;  (global-undo-tree-mode 1)
;;  :config
;;  (setq undo-tree-auto-save-history t
;;        undo-tree-history-directory-alist
;;        `(("." . ,(no-littering-expand-var-file-name "undo-tree-hist/")))))

;; =============================================================================
;;                                   Completion
;; =============================================================================

(use-package vertico
  :ensure t
					; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

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

(use-package aggressive-indent
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;; TODO: setup formatter for elisp and rust
(use-package apheleia
  :ensure t)

;; =============================================================================
;;                                 Odds and Ends
;; =============================================================================

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

;; =============================================================================
;;                                    Keybinds
;; =============================================================================

(load-file (expand-file-name "lib.el"
                             (file-name-directory load-file-name)))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global (kbd "t") 'comment-line)
  (evil-define-key 'normal 'global (kbd "9") (cmd! (scroll-up 18)))
  (evil-define-key 'normal 'global (kbd "0") (cmd! (scroll-down 18)))
  (evil-define-key 'normal 'global (kbd "K") 'evil-next-buffer)
  (evil-define-key 'normal 'global (kbd "L") 'evil-prev-buffer)
  (evil-define-key 'normal 'global (kbd "C-;") 'eval-expression)
  (evil-define-key 'normal eww-mode-map (kbd "H") 'eww-back-url))

