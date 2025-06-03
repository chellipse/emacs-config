;; nano.el --- Init file -*- lexical-binding: t; -*-
;;
;;; Code:

;; (elpaca '(nano-theme
;;           :type git
;;           :host github
;;           :repo "rougier/nano-theme")
;;   (load-theme 'nano-light t))

;; (elpaca nano-modeline
;;   (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
;;   (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
;;   (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
;;   (add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
;;   (add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
;;   (add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
;;   (add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
;;   (add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
;;   (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
;;   (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
;;   (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
;;   (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
;;   (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode))

;; (elpaca mini-frame
;;   (mini-frame-mode))

(elpaca '(nano :type git :host github :repo "rougier/nano-emacs")
  (after! (lsp-mode lsp-ui)
          (setq nano-font-family-monospaced default-font-mono
                nano-font-family-proportional default-font-propo)
          ;; (require 'nano-base-colors)
          ;; (require 'nano-faces)
          ;; (require 'nano-layout)
          ;; (require 'nano-theme)
          ;; (require 'nano-theme-light)
          ;; (require 'nano-defaults)
          ;; (require 'nano-modeline)
          ;; (require 'nano-minibuffer)

          ;; Path to nano emacs modules (mandatory)
          (add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "elpaca/builds/nano"))

          ;; Default layout (optional)
          ;; (require 'nano-layout)

          ;; Theming Command line options (this will cancel warning messages)
          (add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
          (add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
          (add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
          (add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
          (add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
          (add-to-list 'command-switch-alist '("-compact" . (lambda (args))))


          ;; Customize support for 'emacs -q' (Optional)
          ;; You can enable customizations by creating the nano-custom.el file
          ;; with e.g. `touch nano-custom.el` in the folder containing this file.
          ;; (let* ((this-file  (or load-file-name (buffer-file-name)))
          ;;        (this-dir  (file-name-directory this-file))
          ;;        (custom-path  (concat this-dir "nano-custom.el")))
          ;;   (when (and (eq nil user-init-file)
          ;;              (eq nil custom-file)
          ;;              (file-exists-p custom-path))
          ;;     (setq user-init-file this-file)
          ;;     (setq custom-file custom-path)
          ;;     (load custom-file)))

          ;; Theme
          (require 'nano-faces)
          (require 'nano-theme)
          (require 'nano-theme-dark)
          (require 'nano-theme-light)

          (advice-add 'nano-theme--mode-line
                      :override (lambda ()
                                  "Derive mode-line and header-line faces from nano-faces."
                                  (set-face-attribute 'mode-line nil
                                                      ;; :height 0.1
                                                      :foreground (if (display-graphic-p)
                                                                      (face-background 'nano-face-default)
                                                                    (face-foreground 'nano-face-default))
                                                      :background (face-background 'nano-face-default)
                                                      ;; :underline  (if (display-graphic-p)
                                                      ;;                 (face-background 'nano-face-subtle)
                                                      ;;               t)
                                                      :overline nil
                                                      :box nil)
                                  (set-face-attribute 'mode-line-inactive nil
                                                      ;; :height 0.1
                                                      :foreground (if (display-graphic-p)
                                                                      (face-background 'nano-face-default)
                                                                    (face-foreground 'nano-face-default))
                                                      :background (face-background 'nano-face-default)
                                                      ;; :underline (if (display-graphic-p)
                                                      ;;                (face-background 'nano-face-subtle)
                                                      ;;              t)
                                                      :overline nil
                                                      :inherit nil
                                                      :box nil)

                                  (set-face-attribute 'doom-modeline-evil-insert-state nil
                                                      :foreground "#51afef")

                                  ;;(when (display-graphic-p)
                                  (set-face-attribute 'header-line nil
                                                      :weight 'light
                                                      :foreground (face-foreground 'nano-face-default)
                                                      :background (face-background 'nano-face-default)

                                                      :overline nil
                                                      :underline nil
                                                      :box nil
                                                      :box `(:line-width 1
                                                                         :color ,(face-background 'nano-face-default)
                                                                         :style nil)
                                                      :inherit nil)

                                  ;; (when (not (display-graphic-p))
                                  ;;   (set-face-attribute 'header-line nil
                                  ;;                    :weight 'light
                                  ;;                       :foreground (face-foreground 'nano-face-default)
                                  ;;                       :background (face-background 'nano-face-subtle)
                                  ;;                       :inverse-video t
                                  ;;                       :overline nil
                                  ;;                       :underline nil
                                  ;;                       :box nil
                                  ;;                            :inherit nil))

                                  ;; (set-face-attribute 'internal-border nil
                                  ;;                     :background (face-foreground 'nano-face-default))

                                  (set-face-attribute 'internal-border nil
                                                      :background (face-background 'nano-face-default)))
                      )

          (advice-add 'nano-theme-set-light :after
                      (lambda ()
                        (setq nano-color-popout "#98be65"
                              nano-color-critical "#B71C1C")))

          (advice-add 'nano-faces :after
                      (lambda ()
                        (set-face-attribute 'nano-face-critical nil
                                            :foreground nano-color-critical
                                            :background nano-color-background)))

          (advice-add 'nano-theme--term :override
                      (lambda ()
                        "Derive term faces from nano faces, and material theme colors."
                        (with-eval-after-load 'term
                          (set-face 'term-bold                                   'nano-face-strong)
                          (set-face-attribute 'term-color-black nil
                                              :foreground (face-foreground 'nano-face-default)
                                              :background (face-foreground 'nano-face-default))
                          (set-face-attribute 'term-color-white nil
                                              :foreground (face-background 'nano-face-default)
                                              :background (face-background 'nano-face-default))
                          (set-face-attribute 'term-color-red nil
                                              :foreground "#C62828"   ;; material color red 800
                                              :background "#FFCDD2")  ;; material color red L100
                          (set-face-attribute 'term-color-green nil
                                              :foreground "#558B2F"   ;; material color green 800
                                              :background "#C8E6C9")  ;; material color green L100
                          (set-face-attribute 'term-color-yellow nil
                                              :foreground "#6D4C41"    ;; material color yellow 800
                                              :background "#FFF9C4");; material color yellow L100
                          (set-face-attribute 'term-color-blue nil
                                              :foreground "#0277BD"   ;; material color blue 800
                                              :background "#BBDEFB")  ;; material color blue L100
                          (set-face-attribute 'term-color-magenta nil
                                              :foreground "#4527A0"   ;; material color purple 800
                                              :background "#E1BEE7")  ;; material color purple L100
                          (set-face-attribute 'term-color-cyan nil
                                              :foreground "#00838F"   ;; material color cyan 800
                                              :background "#B2EBF2"))))  ;; material color cyan L100

          (custom-set-faces
           '(link ((t (:foreground "deep sky blue"))))
           '(tree-sitter-hl-face:comment ((t (:inherit font-lock-comment-face :slant italic))))
           '(tree-sitter-hl-face:constructor ((t (:inherit nil))))
           '(tree-sitter-hl-face:function.call ((t (:inherit (link font-lock-function-name-face) :underline nil :weight bold))))
           '(tree-sitter-hl-face:function.macro ((t (:inherit font-lock-preprocessor-face :weight bold))))
           '(tree-sitter-hl-face:type ((t (:foreground "#FFAB91")))))

          (advice-add 'refresh-global-font-size :after
                      (lambda (&rest r)
                        (setq nano-font-size font-size)
                        (nano-refresh-theme)))

          (advice-add 'lsp-ui-doc--make-frame :after
                      (lambda (&rest r)
                        (nano-refresh-theme)))


          (cond
           ((member "-default" command-line-args) t)
           ((member "-dark" command-line-args) (nano-theme-set-dark))
           (t (nano-theme-set-light)))
          (call-interactively 'nano-refresh-theme)

          ;; Nano default settings (optional)
          ;; (require 'nano-defaults)

          ;; Nano session saving (optional)
          ;; (require 'nano-session)

          ;; Nano header & mode lines (optional)
          ;; (require 'nano-modeline)

          ;; Nano key bindings modification (optional)
          ;; (require 'nano-bindings)

          ;; Compact layout (need to be loaded after nano-modeline)
          ;; (when (member "-compact" command-line-args)
          ;; (require 'nano-compact))

          ;; Nano counsel configuration (optional)
          ;; Needs "counsel" package to be installed (M-x: package-install)
          ;; (require 'nano-counsel)

          ;; Welcome message (optional)
          ;; (let ((inhibit-message t))
          ;;   (message "Welcome to GNU Emacs / N Λ N O edition")
          ;;   (message (format "Initialization time: %s" (emacs-init-time))))

          ;; Splash (optional)
          ;; (unless (member "-no-splash" command-line-args)
          ;;   (require 'nano-splash))

          ;; Help (optional)
          (unless (member "-no-help" command-line-args)
            (require 'nano-help))
          ))

;;; nano.el ends here
