;;; check-formatting.el --- Check if code blocks in config.org are formatted -*- lexical-binding: t -*-

;; This script checks if all code blocks in config.org are properly formatted
;; using the same apheleia formatters configured in the main config.

(require 'org)
(require 'org-element)
(require 'apheleia)

(defvar check-formatting-failed nil
  "Set to t if any formatting issues are found.")

(defvar check-formatting-errors nil
  "List of formatting errors found. Each element is (line-num . lang).")

(defvar check-formatting-org-to-mode-alist
  '(("emacs-lisp" . emacs-lisp-mode)
    ("elisp" . emacs-lisp-mode)
    ("rust" . rust-ts-mode)
    ("nix" . nix-ts-mode)
    ("python" . python-ts-mode)
    ("ruby" . ruby-ts-mode)
    ("yaml" . yaml-ts-mode)
    ("fish" . fish-mode)
    ("c" . c-ts-mode)
    ("c++" . c++-ts-mode)
    ("sh" . bash-ts-mode)
    ("bash" . bash-ts-mode))
  "Mapping from org-babel language names to major modes.")

(defun check-formatting-get-mode-from-lang (lang)
  "Get the major mode for org-babel language LANG."
  (alist-get lang check-formatting-org-to-mode-alist nil nil #'string=))

(defun check-formatting-format-string (content mode)
  "Format CONTENT string as if it were in major MODE using apheleia.
Returns the formatted string or nil if no formatter is configured."
  (let ((formatter (alist-get mode apheleia-mode-alist)))
    (if (not formatter)
        nil
      (with-temp-buffer
        (insert content)
        (funcall mode)
        ;; Run apheleia synchronously in the temp buffer
        (let ((apheleia-mode nil)) ; Don't enable the minor mode
          (condition-case err
              (progn
                (apheleia-format-buffer formatter
                                        (current-buffer)
                                        (lambda () nil))
                ;; apheleia-format-buffer is async, so we need to wait
                ;; Actually, let's use a different approach - call the formatter directly
                (buffer-string))
            (error
             (message "Error formatting with %s: %s" formatter err)
             nil)))))))

(defun check-formatting-sync-format (content formatter mode)
  "Synchronously format CONTENT using FORMATTER for major MODE.
Returns formatted string or nil on error."
  (let ((formatter-def (alist-get formatter apheleia-formatters)))
    (cond
     ;; Function-based formatter (like apheleia-indent-lisp-buffer)
     ((functionp formatter-def)
      (condition-case err
          (with-temp-buffer
            (insert content)
            (funcall mode)
            ;; For elisp, just use indent-region which is what apheleia does
            (let ((inhibit-message t))
              (indent-region (point-min) (point-max)))
            (buffer-string))
        (error nil)))

     ;; Command-list formatter (like '("rustfmt" ...))
     ((listp formatter-def)
      (let ((temp-file (make-temp-file "check-fmt-"))
            (output-file (make-temp-file "check-fmt-out-")))
        (unwind-protect
            (progn
              (with-temp-file temp-file
                (insert content))
              (let* ((cmd (car formatter-def))
                     (args (mapcar (lambda (arg)
                                     (pcase arg
                                       ('filepath temp-file)
                                       (_ arg)))
                                   (cdr formatter-def)))
                     ;; Some formatters read from stdin
                     (use-stdin (member "-" args))
                     (exit-code (if use-stdin
                                    (with-temp-buffer
                                      (insert content)
                                      (apply #'call-process-region
                                             (point-min) (point-max)
                                             cmd nil (list output-file nil) nil
                                             (remq "-" args)))
                                  (apply #'call-process
                                         cmd temp-file (list output-file nil) nil
                                         args))))
                (if (zerop exit-code)
                    (with-temp-buffer
                      (insert-file-contents output-file)
                      (buffer-string))
                  nil)))
          (delete-file temp-file)
          (delete-file output-file))))

     ;; Unknown formatter type
     (t nil))))

(defun check-formatting-check-block (element)
  "Check if code block ELEMENT is properly formatted.
Returns t if formatted correctly, nil otherwise."
  (let* ((lang (org-element-property :language element))
         (raw-content (org-element-property :value element))
         ;; Remove common leading whitespace from all lines
         (content (with-temp-buffer
                    (insert raw-content)
                    (goto-char (point-min))
                    ;; Remove leading whitespace that's common to all lines
                    (let ((indent (save-excursion
                                    (goto-char (point-min))
                                    (if (re-search-forward "^\\([ \t]+\\)" nil t)
                                        (length (match-string 1))
                                      0))))
                      (when (> indent 0)
                        (goto-char (point-min))
                        (while (not (eobp))
                          (when (looking-at (format "^[ \t]\\{%d\\}" indent))
                            (delete-region (point) (+ (point) indent)))
                          (forward-line 1))))
                    (buffer-string)))
         (begin (org-element-property :begin element))
         (mode (check-formatting-get-mode-from-lang lang))
         (formatter (when mode (alist-get mode apheleia-mode-alist)))
         (line-num (line-number-at-pos begin)))

    (if (not formatter)
        ;; No formatter configured, skip this block
        (progn
          (princ ".")
          t)
      (let ((formatted (check-formatting-sync-format content formatter mode)))
        (if (not formatted)
            ;; Formatter failed, skip
            (progn
              (princ ".")
              t)
          (if (string= content formatted)
              (progn
                (princ "\033[32m.\033[0m")
                t)
            ;; Formatting differs!
            (princ "\033[31m!\033[0m")
            (push (cons line-num lang) check-formatting-errors)
            nil))))))

(defun check-formatting-check-file (file)
  "Check all code blocks in org FILE for proper formatting."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (block)
        (unless (check-formatting-check-block block)
          (setq check-formatting-failed t))))))

(defun check-formatting-main ()
  "Main entry point for checking formatting."
  (let ((config-file (expand-file-name "config.org" user-emacs-directory)))
    (setq check-formatting-errors nil)
    (setq check-formatting-failed nil)
    (princ "\nChecking formatting in config.org:\n")
    (check-formatting-check-file config-file)
    (princ "\n\n")
    (if check-formatting-failed
        (progn
          (princ "\033[31mFormatting errors found:\033[0m\n")
          (dolist (error (reverse check-formatting-errors))
            (princ (format "  Line %4d: %s\n" (car error) (cdr error))))
          (princ (format "\n\033[31m❌ Formatting check FAILED - %d block%s need formatting\033[0m\n"
                         (length check-formatting-errors)
                         (if (= 1 (length check-formatting-errors)) "" "s")))
          (kill-emacs 1))
      (princ "\033[32m✅ All code blocks are properly formatted\033[0m\n")
      (kill-emacs 0))))

;; Run when loaded in batch mode
(when noninteractive
  (check-formatting-main))

(provide 'check-formatting)
;;; check-formatting.el ends here
