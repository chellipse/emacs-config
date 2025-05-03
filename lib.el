;;; lib.el --- Library code -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Various library code, some taken from doom.
;;
;;; Code:

(defvar doom-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

(defun doom-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defun doom--map-keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list 'normal 'visual 'insert). See
`doom-evil-state-alist' to customize this."
  (cl-loop for l across (doom-keyword-name keyword)
           if (assq l doom-evil-state-alist) collect (cdr it)
           else do (error "not a valid state: %s" l)))

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro map! (states mode &rest body)
  "A wrapper around general-evil-define-key."
  `(general-evil-define-key ,(doom--map-keyword-to-states states) ,mode ,@body))

(provide 'lib)

;;; lib.el ends here
