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

(defmacro my-general-evil-define-key (states keymaps prefix &rest args)
  "A wrapper for `general-define-key' that is similar to `evil-define-key'.
It has positional arguments for STATES and KEYMAPS (that will not be overridden
by a later :keymaps or :states argument). Besides this, it acts the same as
`general-define-key', and ARGS can contain keyword arguments in addition to
keybindings. This can basically act as a drop-in replacement for
`evil-define-key', and unlike with `general-define-key', KEYMAPS does not need
to be quoted."
  (declare (indent 2))
  `(general-def
     :states ,(if (and (listp states)
                       (eq (car states) 'quote))
                  `,states
		`',states)
     :keymaps ,(if (and (listp keymaps)
			(eq (car keymaps) 'quote))
                   `,keymaps
                 `',keymaps)
     :prefix ,(if (and (listp prefix)
                       (eq (car prefix) 'quote))
                  `,prefix
		`',prefix)
     ,@args))

(defmacro map! (states mode &rest body)
  "A wrapper around my-general-evil-define-key."
  `(my-general-evil-define-key ,(doom--map-keyword-to-states states) ,mode () ,@body))

(defmacro map-leader! (states mode leader &rest body)
  "A wrapper around my-general-evil-define-key."
  `(my-general-evil-define-key ,(doom--map-keyword-to-states states) ,mode ,leader ,@body))

(provide 'lib)

;;; lib.el ends here
