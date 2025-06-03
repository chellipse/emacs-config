;; doom-modeline.el --- Init file -*- lexical-binding: t; -*-
;;
;;; Code:

(elpaca doom-modeline
  (doom-modeline-mode 1)
  (setq doom-modeline-time-clock-size 3.0
        doom-modeline-always-show-macro-register t
        doom-modeline-buffer-encoding 'non-default
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-height 5)
  ;; (display-time-mode 1)
  (column-number-mode 1))

;;; doom-modeline.el ends here
