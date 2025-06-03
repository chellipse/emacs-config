;; doom.el --- Init file -*- lexical-binding: t; -*-
;;
;;; Code:

(elpaca doom-themes
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  (doom-themes-org-config)
  ;; (after!
  ;;  (tree-sitter)
  ;;  ;; Functions.
  ;;  (set-face-attribute 'tree-sitter-hl-face:function nil
  ;;                      :inherit '(link font-lock-function-name-face)
  ;;                      :underline nil)
  ;;  (set-face-attribute 'tree-sitter-hl-face:function.call nil
  ;;                      :inherit '(link font-lock-function-name-face)
  ;;                      :weight 'bold
  ;;                      :underline nil)
  ;;  (set-face-attribute 'tree-sitter-hl-face:function.builtin nil
  ;;                      :inherit 'font-lock-builtin-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:function.special nil
  ;;                      :inherit 'font-lock-preprocessor-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:function.macro nil
  ;;                      :inherit 'font-lock-preprocessor-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:method nil
  ;;                      :inherit 'tree-sitter-hl-face:function)
  ;;  (set-face-attribute 'tree-sitter-hl-face:method.call nil
  ;;                      :weight 'bold
  ;;                      :inherit 'tree-sitter-hl-face:function.call)

  ;;  ;; Types.
  ;;  (set-face-attribute 'tree-sitter-hl-face:type nil
  ;;                      :inherit 'font-lock-type-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:type.parameter nil
  ;;                      :inherit 'font-lock-variable-name-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:type.argument nil
  ;;                      :inherit 'tree-sitter-hl-face:type)
  ;;  (set-face-attribute 'tree-sitter-hl-face:type.builtin nil
  ;;                      :inherit 'font-lock-builtin-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:type.super nil
  ;;                      :inherit 'tree-sitter-hl-face:type)
  ;;  (set-face-attribute 'tree-sitter-hl-face:constructor nil
  ;;                      :inherit 'tree-sitter-hl-face:type)

  ;;  ;; Variables, properties.
  ;;  (set-face-attribute 'tree-sitter-hl-face:variable nil
  ;;                      :inherit 'font-lock-variable-name-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:variable.parameter nil
  ;;                      :inherit 'tree-sitter-hl-face:variable)
  ;;  (set-face-attribute 'tree-sitter-hl-face:variable.builtin nil
  ;;                      :inherit 'font-lock-builtin-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:variable.special nil
  ;;                      :inherit 'font-lock-warning-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:property nil
  ;;                      :inherit 'font-lock-constant-face
  ;;                      :slant 'normal)
  ;;  (set-face-attribute 'tree-sitter-hl-face:property.definition nil
  ;;                      :inherit 'tree-sitter-hl-face:variable.parameter)

  ;;  ;; Strings, comments, text proses.
  ;;  (set-face-attribute 'tree-sitter-hl-face:comment nil
  ;;                      :slant 'italic
  ;;                      :inherit 'font-lock-comment-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:doc nil
  ;;                      :inherit 'font-lock-doc-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:string nil
  ;;                      :inherit 'font-lock-string-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:string.special nil
  ;;                      :inherit 'tree-sitter-hl-face:string
  ;;                      :weight 'bold)
  ;;  (set-face-attribute 'tree-sitter-hl-face:escape nil
  ;;                      :inherit 'font-lock-keyword-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:embedded nil
  ;;                      :inherit 'default)

  ;;  ;; Atomics, constants.
  ;;  (set-face-attribute 'tree-sitter-hl-face:keyword nil
  ;;                      :inherit 'font-lock-keyword-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:operator nil
  ;;                      :inherit 'tree-sitter-hl-face:keyword)
  ;;  (set-face-attribute 'tree-sitter-hl-face:label nil
  ;;                      :inherit 'font-lock-preprocessor-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:constant nil
  ;;                      :inherit 'font-lock-constant-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:constant.builtin nil
  ;;                      :inherit 'font-lock-builtin-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:number nil
  ;;                      :inherit 'tree-sitter-hl-face:constant)

  ;;  ;; ------------------------------------
  ;;  ;; Punctuations (aka. should-be-dimmed).

  ;;  (set-face-attribute 'tree-sitter-hl-face:punctuation nil
  ;;                      :inherit 'default)
  ;;  (set-face-attribute 'tree-sitter-hl-face:punctuation.bracket nil
  ;;                      :inherit 'tree-sitter-hl-face:punctuation)
  ;;  (set-face-attribute 'tree-sitter-hl-face:punctuation.delimiter nil
  ;;                      :inherit 'tree-sitter-hl-face:punctuation)
  ;;  (set-face-attribute 'tree-sitter-hl-face:punctuation.special nil
  ;;                      :inherit 'tree-sitter-hl-face:keyword)

  ;;  ;; Markups.
  ;;  (set-face-attribute 'tree-sitter-hl-face:tag nil
  ;;                      :inherit 'font-lock-builtin-face)
  ;;  (set-face-attribute 'tree-sitter-hl-face:attribute nil
  ;;                      :inherit 'font-lock-preprocessor-face))
  )

;;; doom.el ends here
