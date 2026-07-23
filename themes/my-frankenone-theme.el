;;; my-frankenone-theme.el --- doom-one, as left by the theme-switch crash -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; The *formula* behind the accidental "half-themed" state, reverse-engineered
;; from a full face capture (kept as `my-frankenone-capture' for reference).
;;
;; The state = doom-one, mutated by a partial `consult-theme' crash (the gnus
;; :inherit cycle aborted face recalcs partway), which had three effects:
;;
;;   1. BASE: the vast majority of faces are still plain doom-one -- so we
;;      clone doom-one's palette and face list verbatim below.
;;   2. REVERTED: a set of core `defface' faces (the whole font-lock family,
;;      plus error/warning/success/region/... and the ansi-color set) fell back
;;      to Emacs's built-in defaults. Because THIS is a standalone theme (not
;;      layered on doom-one), we reproduce that simply by resetting them to
;;      their `face-default-spec' -- see the `reverted' loop near the bottom.
;;   3. STRANDED: a handful of faces kept values from OTHER themes that were
;;      being previewed when the switch aborted (an orchid cursor; Material/
;;      Palenight org headings; a doom-scaled outline set; a gruvbox isearch;
;;      leftover highlight/fringe/mode-line). These are irreducible data, set
;;      explicitly in the `Stranded leftovers' block.
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

(defgroup my-frankenone-theme nil
  "Options for the `my-frankenone' theme."
  :group 'doom-themes)

(defcustom my-frankenone-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line."
  :group 'my-frankenone-theme
  :type '(choice integer boolean))

(def-doom-theme my-frankenone
  "doom-one left half-reverted by a partial theme switch."
  :family 'doom-one
  :background-mode 'dark

  ;; --- doom-one palette, verbatim (this is the BASE) ---
  ((bg         '("#282c34" "black"       "black"  ))
   (fg         '("#bbc2cf" "#bfbfbf"     "brightwhite"  ))
   (bg-alt     '("#21242b" "black"       "black"        ))
   (fg-alt     '("#5B6268" "#2d2d2d"     "white"        ))
   (base0      '("#1B2229" "black"       "black"        ))
   (base1      '("#1c1f24" "#1e1e1e"     "brightblack"  ))
   (base2      '("#202328" "#2e2e2e"     "brightblack"  ))
   (base3      '("#23272e" "#262626"     "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f"     "brightblack"  ))
   (base5      '("#5B6268" "#525252"     "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b"     "brightblack"  ))
   (base7      '("#9ca0a4" "#979797"     "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf"     "white"        ))
   (grey       base4)
   (red        '("#ff6c6b" "#ff6655" "red"          ))
   (orange     '("#da8548" "#dd8844" "brightred"    ))
   (green      '("#98be65" "#99bb66" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ECBE7B" "#ECBE7B" "yellow"       ))
   (blue       '("#51afef" "#51afef" "brightblue"   ))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (magenta    '("#c678dd" "#c678dd" "brightmagenta"))
   (violet     '("#a9a1e1" "#a9a1e1" "magenta"      ))
   (cyan       '("#46D9FF" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       base5)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (doom-darken bg-alt 0.1))
   (modeline-bg-alt          `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg)))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))
   (-modeline-pad
    (when my-frankenone-padded-modeline
      (if (integerp my-frankenone-padded-modeline) my-frankenone-padded-modeline 4))))

  ;;;; doom-one structural faces we keep + the stranded leftovers
  (((line-number-current-line &override) :foreground fg)
   (solaire-mode-line-face
    :inherit 'mode-line :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))

   ;;;; REVERTED: core faces that fell back to Emacs's built-in defaults.
   ;; (These are the Emacs dark-frame defface values, transcribed so they
   ;; reliably override doom's -- a standalone theme's facelist wins here.)
   (font-lock-keyword-face       :foreground "Cyan1")
   (font-lock-type-face          :foreground "PaleGreen")
   (font-lock-string-face        :foreground "LightSalmon")
   (font-lock-doc-face           :foreground "LightSalmon")
   (font-lock-comment-face       :foreground "chocolate1")
   (font-lock-comment-delimiter-face :foreground "chocolate1")
   (font-lock-function-name-face :foreground "LightSkyBlue")
   (font-lock-function-call-face :foreground "LightSkyBlue")
   (font-lock-variable-name-face :foreground "LightGoldenrod")
   (font-lock-variable-use-face  :foreground "LightGoldenrod")
   (font-lock-property-name-face :foreground "LightGoldenrod")
   (font-lock-property-use-face  :foreground "LightGoldenrod")
   (font-lock-constant-face      :foreground "Aquamarine")
   (font-lock-builtin-face       :foreground "LightSteelBlue")
   (font-lock-preprocessor-face  :foreground "LightSteelBlue")
   (font-lock-warning-face       :foreground "Pink" :weight 'bold)
   (font-lock-escape-face        :weight 'bold)
   (error       :foreground "Pink" :weight 'bold)
   (warning     :foreground "DarkOrange" :weight 'bold)
   (success     :foreground "Green1" :weight 'bold)
   (shadow      :foreground "grey70")
   (region      :background "blue3" :extend t)
   (lazy-highlight :background "paleturquoise4")
   (match       :background "RoyalBlue3")
   (secondary-selection :background "SkyBlue4" :extend t)
   (minibuffer-prompt :foreground "cyan")
   (escape-glyph :foreground "cyan")
   (nobreak-space :foreground "cyan" :underline t)
   (trailing-whitespace :background "red1")
   (tooltip     :foreground "black" :background "lightyellow")
   ;; doom-themes-base has `lsp-ui-doc-background' inherit `tooltip', so the
   ;; reverted lightyellow tooltip leaked into the lsp-ui-doc child frame.
   ;; Point it at doom-one's intended dark popup background instead.
   (lsp-ui-doc-background :background bg-alt :foreground fg)
   (line-number :foreground "grey70")
   (line-number-current-line :foreground "grey70")
   (header-line :height 0.9 :foreground "grey90" :background "grey20"
                :box '(:line-width -1 :color "grey20" :style released-button))
   (tab-line    :height 0.9 :foreground "white" :background "grey20")
   (vertical-border :background "dim gray")
   (window-divider :background "dim gray")
   (window-divider-first-pixel :background "dim gray")
   (window-divider-last-pixel :background "dim gray")
   (bold-italic :weight 'bold :slant 'italic)
   ;; ANSI palette reverted to Emacs's ansi-color defaults
   (ansi-color-black   :foreground "black"   :background "black")
   (ansi-color-red     :foreground "red3"    :background "red3")
   (ansi-color-green   :foreground "green3"  :background "green3")
   (ansi-color-yellow  :foreground "yellow3" :background "yellow3")
   (ansi-color-blue    :foreground "blue2"   :background "blue2")
   (ansi-color-magenta :foreground "magenta3" :background "magenta3")
   (ansi-color-cyan    :foreground "cyan3"   :background "cyan3")
   (ansi-color-white   :foreground "grey90"  :background "gray90")
   (ansi-color-bright-black   :foreground "gray30"   :background "gray30")
   (ansi-color-bright-red     :foreground "red2"     :background "red2")
   (ansi-color-bright-green   :foreground "green2"   :background "green2")
   (ansi-color-bright-yellow  :foreground "yellow2"  :background "yellow2")
   (ansi-color-bright-blue    :foreground "blue1"    :background "blue1")
   (ansi-color-bright-magenta :foreground "magenta2" :background "magenta2")
   (ansi-color-bright-cyan    :foreground "cyan2"    :background "cyan2")
   (ansi-color-bright-white   :foreground "white"    :background "white")

   ;;;; gnus <built-in> -- break the news-low/news-low-empty inheritance cycle
   ;; that doom-themes-base ships (issue #875), which trips Emacs 31's cycle
   ;; guard during per-frame face recalc (corfu child frames, server clients).
   ;; Same fix as `my-doom-laserwave'; needed here since this is a doom theme.
   (gnus-group-news-low       :inherit 'gnus-group-mail-1 :foreground base5)
   (gnus-group-news-low-empty :foreground base5)

   ;;;; Stranded leftovers (values from themes previewed during the crash)
   (cursor      :background "orchid")
   (highlight   :foreground "Old Lace" :background "gray10")
   (fringe      :foreground "Wheat" :background "grey30")
   ;; mode-line deliberately restored to doom-one's normal (dark, readable)
   ;; instead of the stranded grey75/Blue leftover.
   (mode-line   :background modeline-bg :foreground modeline-fg
                :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive :background modeline-bg-inactive :foreground modeline-fg-alt
                       :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground highlight)
   (isearch     :weight 'bold :foreground "#0d1011" :background "#fe8019")
   ;; Material/Palenight org headings
   (org-document-title :height 1.25 :weight 'bold :foreground "#c678dd")
   (org-level-1 :height 1.2  :weight 'bold :foreground "#c3e88d")
   (org-level-2 :height 1.15 :weight 'bold :foreground "#ffcb6b")
   (org-level-3 :height 1.1  :weight 'bold :foreground "#f57373")
   (org-level-4 :height 1.05 :weight 'bold :foreground "#89DDFF")
   (org-level-5 :weight 'bold :foreground "#82aaff")
   (org-level-6 :weight 'bold :foreground "#c792ea")
   (org-level-7 :weight 'bold :foreground "#44b9b1")
   (org-level-8 :weight 'bold :foreground "#bb80b3")
   ;; doom-scaled outline set
   (outline-1 :height 1.8   :weight 'bold :slant 'italic :foreground "#51afef" :extend t)
   (outline-2 :height 2.16  :weight 'bold :slant 'italic :foreground "#c678dd" :extend t)
   (outline-3 :height 2.376 :weight 'bold :slant 'italic :foreground "#a9a1e1" :extend t)
   (outline-4 :height 2.376 :weight 'bold :slant 'italic :foreground "#7cc3f3" :extend t)
   (outline-5 :height 2.376 :weight 'bold :slant 'italic :foreground "#d499e5" :extend t)
   (outline-6 :height 2.376 :weight 'bold :slant 'italic :foreground "#a8d7f7" :extend t)
   (outline-7 :height 2.376 :weight 'bold :slant 'italic :foreground "#e2bbee" :extend t)
   (outline-8 :height 2.376 :weight 'bold :slant 'italic :foreground "#dceffb" :extend t))

  ;;;; Base theme variable overrides
  ())

(provide-theme 'my-frankenone)
;;; my-frankenone-theme.el ends here
