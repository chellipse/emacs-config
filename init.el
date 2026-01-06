;; init.el --- Init file -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; ...
;;
;;; Code:

(set 'gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (run-at-time "2" nil (lambda () (set 'gc-cons-threshold (* 32 1024 1024))))))

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(set 'bidi-inhibit-bpa t)

(set 'auto-mode-case-fold nil)

(let ((old-file-name-handler-alist file-name-handler-alist))
  (set 'file-name-handler-alist (list (rassq 'jka-compr-handler file-name-handler-alist)))
  (add-hook 'after-init-hook
            #'(lambda () (set 'file-name-handler-alist old-file-name-handler-alist))))

(set 'native-comp-speed 3)
(set 'native-comp-async-report-warnings-errors nil)
(set 'native-comp-jit-compilation t)
(set 'native-compile-prune-cache t)

;; Need to load the newer non-builtin version of transient before we change load order
;; else "rg" will complain about new transient endpoints being undefined
(require 'transient)
(let* ((emacs-lisp-path (seq-find (lambda (s) (string-suffix-p "lisp/emacs-lisp" s)) load-path))
       (url-path (seq-find (lambda (s) (string-suffix-p "lisp/url" s)) load-path))
       (lisp-path (directory-file-name (file-name-directory emacs-lisp-path)))
       (old-load-path (mapcar #'copy-sequence load-path)))
  (when emacs-lisp-path
    (delete emacs-lisp-path load-path)
    (delete lisp-path load-path)
    (delete url-path load-path)
    (set 'load-path (append (list emacs-lisp-path lisp-path url-path) load-path))
    (add-hook 'after-init-hook #'(lambda ()(set 'load-path old-load-path)))))

(when (boundp 'pgtk-wait-for-event-timeout)
  (set 'pgtk-wait-for-event-timeout 0.0001))

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(set 'menu-bar-mode nil)
(set 'tool-bar-mode nil)
(set 'scroll-bar-mode nil)

(set 'load-prefer-newer noninteractive)

(when (not after-init-time)  ; nil until after-init-hook has run
  (setq-default inhibit-redisplay t
                inhibit-message (not init-file-debug))
  (defun athame-init--reset-inhibited-vars-h ()
    (setq-default inhibit-redisplay nil inhibit-message nil)
    (remove-hook 'after-init-hook #'athame-init--reset-inhibited-vars-h))
  (add-hook 'after-init-hook #'athame-init--reset-inhibited-vars-h -100))

(setq-default inhibit-x-resources t
              frame-inhibit-implied-resize t)

(set 'inhibit-startup-screen t)
(set 'inhibit-startup-echo-area-message user-login-name)

(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

(setq-default cursor-in-non-selected-windows nil)
(set 'highlight-nonselected-windows nil)
(set 'redisplay-skip-fontification-on-input t)

(require 'use-package)
(require 'org)

;; TODO remove at some point
(setq use-package-compute-statistics t)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;;; init.el ends here
