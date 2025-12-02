;; init.el --- Init file -*- lexical-binding: nil; -*-
;;; Commentary:
;;
;; ...
;;
;;; Code:

(require 'use-package)
(require 'org)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;;; init.el ends here
