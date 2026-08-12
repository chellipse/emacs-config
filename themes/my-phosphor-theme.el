;;; my-phosphor-theme.el --- monochrome terminal theme from one base color -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; A dark-room "hacking in low light" theme: a single BASE color drives the
;; whole palette. Syntax categories are spread across shades/hues of the base
;; (via HSL), the background is a near-black desaturated version of it, and
;; errors/warnings are fixed red/amber -- the only non-base accents, so
;; mistakes actually stand out.
;;
;; TEMPLATE USAGE: this file generates a full doom theme from `base' below.
;; To spin up a different mono theme, copy this file, rename `my-phosphor'
;; -> `my-<name>' throughout, and change the one `base' value (e.g. "#00a2ff"
;; for a blue terminal, "#ff9900" for amber). Everything re-derives.
;;
;; Copied from `my-doom-laserwave' -- keeps its structural overrides and the
;; gnus inheritance-cycle fix.
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)
(require 'color)


;;
;;; Palette derivation helpers (HSL, so they work for any base color)

(defun my-mono--hex-to-rgb (hex)
  "Parse \"#rrggbb\" HEX to a list of three 0..1 floats.
Deliberately not `color-name-to-rgb', which is display-dependent (it
quantises to the frame's color depth, breaking in batch/low-color)."
  (list (/ (string-to-number (substring hex 1 3) 16) 255.0)
        (/ (string-to-number (substring hex 3 5) 16) 255.0)
        (/ (string-to-number (substring hex 5 7) 16) 255.0)))

(defun my-mono--as-doom (hex)
  "Wrap HEX as a doom color list (same value for all display classes)."
  (list hex hex hex))

(defun my-mono-tint (hex &optional dh dl ds)
  "Shift HEX by DH degrees hue, DL lightness, DS saturation (all relative).
Returns a doom color list.  Used for the syntax accents."
  (pcase-let ((`(,h ,s ,l) (apply #'color-rgb-to-hsl (my-mono--hex-to-rgb hex))))
    (my-mono--as-doom
     (apply #'color-rgb-to-hex
            (append (color-hsl-to-rgb (mod (+ h (/ (or dh 0) 360.0)) 1.0)
                                      (min 1.0 (max 0.0 (+ s (or ds 0))))
                                      (min 1.0 (max 0.0 (+ l (or dl 0)))))
                    '(2))))))

(defun my-mono-shade (hex l &optional s dh)
  "Return HEX at absolute lightness L, optional absolute saturation S, hue +DH.
Returns a doom color list.  Used for the desaturated structural ramp."
  (pcase-let ((`(,h ,os ,_) (apply #'color-rgb-to-hsl (my-mono--hex-to-rgb hex))))
    (my-mono--as-doom
     (apply #'color-rgb-to-hex
            (append (color-hsl-to-rgb (mod (+ h (/ (or dh 0) 360.0)) 1.0)
                                      (or s os) l)
                    '(2))))))


;;
;;; Variables

(defgroup my-phosphor-theme nil
  "Options for the `my-phosphor' theme."
  :group 'doom-themes)

(defcustom my-phosphor-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'my-phosphor-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme my-phosphor
    "A monochrome terminal theme derived from a single base color."

  ;; ─── the one knob ───────────────────────────────────────────────
  ((base       "#00dd00")

   ;; structural ramp: desaturated shades of BASE, dark -> light
   (bg         (my-mono-shade base 0.06 0.28))
   (bg-alt     (my-mono-shade base 0.04 0.28))
   (base0      (my-mono-shade base 0.03 0.25))
   (base1      (my-mono-shade base 0.07 0.28))
   (base2      (my-mono-shade base 0.10 0.28))
   (base3      (my-mono-shade base 0.14 0.30))
   (base4      (my-mono-shade base 0.22 0.30))
   (base5      (my-mono-shade base 0.32 0.32))
   (base6      (my-mono-shade base 0.32 0.65))   ; doom-modeline accent bar
   (base7      (my-mono-shade base 0.42 0.32))   ; dim markup
   (base8      (my-mono-shade base 0.85 0.40))
   (fg-alt     (my-mono-shade base 0.36 0.90))
   (fg         (my-mono--as-doom base))           ; exact base (softened, not round-tripped)

   (grey       base4)
   ;; fixed accents (base-independent, so problems always contrast)
   (red        (my-mono--as-doom "#ff5c5c"))
   (orange     (my-mono--as-doom "#ffb000"))
   (green      (my-mono--as-doom "#00e05a"))       ; success / vc-added
   (yellow     (my-mono--as-doom "#ffcf5f"))       ; warning
   ;; base-derived syntax hues (spread around BASE)
   (teal       (my-mono-tint base 30 0.08))
   (blue       (my-mono-tint base 25 0.17))       ; keywords / operators
   (dark-blue  (my-mono-shade base 0.16 0.50))    ; selection
   (magenta    (my-mono-tint base 37 0.07))       ; functions / highlight
   (violet     (my-mono-tint base 24 0.31))       ; constants
   (cyan       (my-mono-tint base 40 0.25))       ; strings / methods
   (dark-cyan  (my-mono-tint base 30 -0.05 -0.30))

   ;; face categories -- required for all themes
   (highlight      magenta)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (my-mono-tint base 23 -0.04 -0.60))
   (doc-comments   (my-mono-tint base 30 0.06 -0.45))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           (my-mono-tint base -27 0.25))
   (strings        cyan)
   (variables      fg)
   (numbers        (my-mono-tint base 12 0.30 -0.38))
   (region         (my-mono-shade base 0.15 0.50))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     bg)
   (-modeline-pad
    (when my-phosphor-padded-modeline
      (if (integerp my-phosphor-padded-modeline)
          my-phosphor-padded-modeline 4)))

   ;; dark modeline + base-colored text (readable, terminal-ish)
   (modeline-fg     fg)
   (modeline-fg-alt base7)
   (modeline-bg base2)
   (modeline-bg-inactive base1))


  ;;;; Base theme face overrides
  ((lazy-highlight :background (doom-darken magenta 0.4) :foreground fg)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad
             `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground highlight)
   (mode-line-highlight :background orange :foreground bg :weight 'bold)
   ;;;; centaur-tabs
   (centaur-tabs-active-bar-face :background magenta)
   (centaur-tabs-modified-marker-selected
    :inherit 'centaur-tabs-selected :foreground magenta)
   (centaur-tabs-modified-marker-unselected
    :inherit 'centaur-tabs-unselected :foreground magenta)
   ;;;; company
   (company-box-background :foreground fg :background bg-alt)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background base6)
   (doom-modeline-info :inherit 'success)
   (doom-modeline-urgent :inherit 'error)
   (doom-modeline-warning :inherit 'warning)
   (doom-modeline-debug :foreground base7)
   (doom-modeline-buffer-minor-mode :foreground base7 :weight 'bold)
   (doom-modeline-project-dir :foreground highlight :weight 'bold)
   (doom-modeline-project-parent-dir :foreground highlight :weight 'bold)
   (doom-modeline-persp-name :foreground highlight :weight 'bold)
   (doom-modeline-buffer-file :foreground fg :weight 'bold)
   (doom-modeline-buffer-modified :foreground orange :weight 'bold)
   (doom-modeline-lsp-success :inherit 'success :weight 'bold)
   (doom-modeline-buffer-path :foreground highlight :weight 'bold)
   (doom-modeline-buffer-project-root :foreground highlight)
   (doom-modeline-evil-visual-state :foreground yellow)
   (doom-modeline-evil-replace-state :foreground orange)
   (doom-modeline-evil-operator-state :foreground teal)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background base3 :foreground base7)

   ;;;; gnus <built-in>
   ;; Break the news-low/news-low-empty inheritance cycle: doom-themes has `-empty'
   ;; inherit `-low' while built-in gnus inherits the other way, which trips Emacs
   ;; 31's cycle guard during per-frame face recalc (e.g. corfu child frames).
   ;; Remove once https://github.com/doomemacs/themes/issues/875 is fixed and we're
   ;; on that doom-themes version.
   (gnus-group-news-low       :inherit 'gnus-group-mail-1 :foreground base5)
   (gnus-group-news-low-empty :foreground base5)

   ;;;; ivy
   (ivy-current-match :background base2 :distant-foreground nil)
   ;;;; markdown-mode
   (markdown-header-delimiter-face :foreground base7)
   (markdown-metadata-key-face     :foreground base7)
   (markdown-list-face             :foreground base7)
   (markdown-link-face             :foreground cyan)
   (markdown-url-face              :inherit 'link :foreground fg :weight 'normal)
   (markdown-italic-face           :inherit 'italic :foreground magenta)
   (markdown-bold-face             :inherit 'bold :foreground magenta)
   (markdown-markup-face           :foreground base7)
   (markdown-gfm-checkbox-face :foreground cyan)
   ;;;; mic-paren
   (paren-face-match
    :foreground yellow :background (doom-darken bg 0.2) :weight 'ultra-bold)
   ;;;; outline <built-in> -- colors kept green; heading sizes/italic/extend
   ;; kept from `my-frankenone' (a previewed theme's outline scaling, visible
   ;; in outline-mode and markdown headers, which inherit these).
   ((outline-1 &override) :foreground blue :height 1.8 :slant 'italic :extend t)
   ((outline-2 &override) :foreground green :height 2.16 :slant 'italic :extend t)
   ((outline-3 &override) :foreground teal :height 2.376 :slant 'italic :extend t)
   ((outline-4 &override) :foreground (doom-darken blue 0.2) :height 2.376 :slant 'italic :extend t)
   ((outline-5 &override) :foreground (doom-darken green 0.2) :height 2.376 :slant 'italic :extend t)
   ((outline-6 &override) :foreground (doom-darken teal 0.2) :height 2.376 :slant 'italic :extend t)
   ((outline-7 &override) :foreground (doom-darken blue 0.4) :height 2.376 :slant 'italic :extend t)
   ((outline-8 &override) :foreground (doom-darken green 0.4) :height 2.376 :slant 'italic :extend t)
   ;;;; org <built-in>
   ((org-block &override) :background base2)
   ((org-block-begin-line &override) :background base2)
   (org-hide :foreground hidden)
   (org-todo :foreground cyan :bold 'inherit)
   ((org-document-title &override) :height 1.25)   ; #+TITLE size kept from frankenone
   ;; Org headings kept from `my-frankenone' (Material/Palenight colors +
   ;; sizes) -- doom leaves org-level-* inheriting outline, so they need to
   ;; be set natively here to survive standalone.
   (org-level-1 :height 1.2  :weight 'bold :foreground "#c3e88d")
   (org-level-2 :height 1.15 :weight 'bold :foreground "#ffcb6b")
   (org-level-3 :height 1.1  :weight 'bold :foreground "#f57373")
   (org-level-4 :height 1.05 :weight 'bold :foreground "#89DDFF")
   (org-level-5 :weight 'bold :foreground "#82aaff")
   (org-level-6 :weight 'bold :foreground "#c792ea")
   (org-level-7 :weight 'bold :foreground "#44b9b1")
   (org-level-8 :weight 'bold :foreground "#bb80b3")
   ;;;; org-pomodoro
   (org-pomodoro-mode-line :inherit 'mode-line-emphasis :weight 'bold)
   (org-pomodoro-mode-line-overtime :inherit 'org-pomodoro-mode-line)
   (org-pomodoro-mode-line-break :inherit 'org-pomodoro-mode-line)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))))

;;; my-phosphor-theme.el ends here
