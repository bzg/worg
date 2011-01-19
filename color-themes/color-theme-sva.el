;;; color-theme-sva.el --- my color theme

;; Copyright (C) 2003-2010 Sebastien Vauban
;; Time-stamp: <2010-10-07 Thu 10:53 sva on mundaneum>

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this file; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;* Code:

(require 'color-theme)

(defun color-theme-sva ()
  "Color theme by Sebastien Vauban."
  (interactive)
  (let*
      ;; inherit properties, independently of definition order
      ((ol1 '(:family "Arial" :height 1.8 :weight bold :box (:line-width 1 :color "#0000CC") :foreground "cornflower blue" :background "LightSteelBlue1"))
       (ol2 '(:family "Arial" :height 1.6 :weight bold :box (:line-width 1 :color "#00CC00") :foreground "lime green" :background "DarkSeaGreen1"))
       (ol3 '(:family "Arial" :height 1.3 :weight bold :foreground "#F9A049"))
       (ol4 '(:family "Arial" :height 1.2 :weight bold :foreground "red3"))
       (ol5 '(:family "Arial" :height 1.1 :weight bold :foreground "cornflower blue"))
       (ol6 '(:family "Arial" :height 1.1 :weight bold :foreground "lime green"))
       (ol7 '(:family "Arial" :height 1.1 :weight bold :foreground "#F9A049"))
       (ol8 '(:family "Arial" :height 1.1 :weight bold :foreground "red3"))
       (inline-code '(:foreground "blue1" :background "#EAFFEA"))
       (block-code '(:foreground "blue1" :background "#FFFFEA"))
       (directory '(:weight bold :foreground "blue" :background "#FFF58F"))
       (marked-line '(:weight bold :foreground "white" :background "red"))
       (clock-line '(:box (:line-width 1 :color "#9EABB6") :foreground "black" :background "#BFBF00"))
       (column '(:height 1.0 :weight normal :slant normal :underline nil :strike-through nil :box (:line-width 1 :color "black") :foreground "black" :background "#EEE8AA"))
       (symlink '(:foreground "deep sky blue")))
    (color-theme-install
     `(color-theme-sva
       ;; frame parameters
       ((background-color . "white")
        (background-mode . light)
        (border-color . "black")
        (cursor-color . "#15FF00")
        (foreground-color . "black")
        (mouse-color . "black"))

       ;; faces
       (default ((t (nil))))
       (bold ((t (:weight bold))))
       (bold-italic ((t (:weight bold :slant italic))))
       (italic ((t (:slant italic))))
       (underline ((t (:underline t))))

       ;; anything
       (anything-header ((t (:family "Sans Serif" :height 1.3 :weight bold :foreground "white" :background "#666699"))))
       ;; under test
       (anything-bookmarks-su-face ((t (:foreground "red"))))
       (anything-dir-heading ((t (:foreground "blue" :background "pink"))))
       (anything-dir-priv ((t (:foreground "dark red" :background "light grey"))))
       (anything-file-name ((t (:foreground "blue"))))
       (anything-gentoo-match-face ((t (:foreground "red"))))
       (anything-isearch-match ((t (:background "yellow"))))
       (anything-overlay-line-face ((t (:underline t :foreground "white" :background "IndianRed4"))))
       (anything-visible-mark ((t (:foreground "black" :background "green1"))))
       (anything-w3m-bookmarks-face ((t (:underline t :foreground "cyan1"))))

       ;; bbdb
       (bbdb-company ((t (:slant italic :foreground "steel blue"))))
       (bbdb-field-name ((t (:weight bold :foreground "steel blue"))))
       (bbdb-field-value ((t (:foreground "steel blue"))))
       (bbdb-name ((t (:underline t :foreground "#FF6633"))))

       ;; browse-kill-ring
       (browse-kill-ring-separator-face ((t (:weight bold :foreground "slate gray"))))

       ;; calendar
       (calendar-today-face ((t (:weight bold :background "#CCCCFF")))) ; "yellow"
       (diary-face ((t (:foreground "#87C9FC"))))  ;"dark cyan"
       (holiday-face ((t (:background "#B6B2AE"))))  ; "red"

       ;; IRC
       (circe-highlight-all-nicks-face ((t (:foreground "blue"))))  ; other nicks
       (circe-highlight-nick-face ((t (:foreground "firebrick"))))  ; message with my nick cited
       (circe-my-message-face ((t (:foreground "red"))))
       (circe-originator-face ((t (:foreground "blue"))))

       ;; column-marker
       (column-marker-1-face ((t (:background "DarkSeaGreen1"))))
       (column-marker-2-face ((t (:background "lemon chiffon"))))
       (column-marker-3-face ((t (:background "misty rose"))))

       (comint-highlight-input ((t (:weight bold))))
       (comint-highlight-prompt ((t (:foreground "blue"))))

       (css-selector ((t (:weight bold :foreground "blue"))))

       ;; custom
       (custom-button-pressed-face ((t (:foreground "black" :background "light grey" :box (:line-width 2 :style pressed-button)))))
       (custom-changed-face ((t (:foreground "white" :background "blue"))))
       (custom-face-tag-face ((t (:underline t))))
       (custom-invalid-face ((t (:foreground "yellow" :background "red"))))
       (custom-modified-face ((t (:foreground "white" :background "blue"))))
       (custom-rogue-face ((t (:foreground "pink" :background "black"))))
       (custom-saved-face ((t (:underline t))))
       (custom-set-face ((t (:foreground "blue" :background "white"))))
       (custom-state-face ((t (:foreground "green4"))))
       (custom-variable-button-face ((t (:weight bold :underline t))))

       ;; cvs
       (cvs-filename-face ((t (:foreground "blue4"))))
       (cvs-handled-face ((t (:foreground "pink"))))
       (cvs-header-face ((t (:weight bold :foreground "blue4"))))
       (cvs-marked-face ((t (:weight bold :foreground "green3"))))
       (cvs-msg-face ((t (:slant italic :foreground "gray55"))))
       (cvs-need-action-face ((t (:foreground "orange"))))
       (cvs-unknown-face ((t (:foreground "red"))))

       ;; diff
       (diff-added ((t (:foreground "#2B7E2A" :background "#DDFFDD"))))
       (diff-file-header ((t (:foreground "red" :background "#EAF2F5"))))
       (diff-header ((t (:foreground "#999999" :background "#EAF2F5"))))
       (diff-hunk-header ((t (:foreground "#999999" :background "#EAF2F5"))))
       (diff-index ((t (:foreground "#4183C4" :background "#EAF2F5"))))
       (diff-indicator-added ((t (:background "#AAFFAA"))))
       (diff-indicator-removed ((t (:background "#FFAAAA"))))
       (diff-removed ((t (:foreground "dark magenta" :background "#FFDDDD"))))

       ;; dircolors
       (dircolors-face-asm ((t (:foreground "black"))))
       (dircolors-face-backup ((t (:foreground "black"))))
       (dircolors-face-compress ((t (:foreground "red"))))
       (dircolors-face-dir ((t ,directory)))
       (dircolors-face-doc ((t (:foreground "black"))))
       (dircolors-face-dos ((t (:foreground "green3"))))
       (dircolors-face-emacs ((t (:foreground "black"))))
       (dircolors-face-exec ((t (:foreground "green3"))))
       (dircolors-face-html ((t (:foreground "black"))))
       (dircolors-face-img ((t (:foreground "black"))))
       (dircolors-face-lang ((t (:foreground "black"))))
       (dircolors-face-lang-interface ((t (:foreground "black"))))
       (dircolors-face-make ((t (:foreground "black"))))
       (dircolors-face-objet ((t (:foreground "black"))))
       (dircolors-face-package ((t (:foreground "red"))))
       (dircolors-face-paddb ((t (:foreground "black"))))
       (dircolors-face-ps ((t (:foreground "black"))))
       (dircolors-face-sound ((t (:foreground "black"))))
       (dircolors-face-tar ((t (:foreground "red"))))
       (dircolors-face-text ((t (:foreground "black"))))
       (dircolors-face-yacc ((t (:foreground "black"))))

       ;; dired
       (dired-face-boring ((t (:foreground "gray65"))))
       (dired-face-directory ((t (:weight bold))))
       (dired-face-executable ((t (:foreground "green3"))))
       (dired-face-flagged ((t (:weight bold :foreground "white" :background "red"))))
       (dired-face-header ((t (:foreground "black" :background "gray75"))))
       (dired-face-marked ((t (:background "pale violet red"))))
       (dired-face-permissions ((t (:foreground "black" :background "gray75"))))
       (dired-face-setuid ((t (:foreground "red"))))
       (dired-face-socket ((t (:foreground "magenta"))))
       (dired-face-symlink ((t ,symlink)))

       ;; dired+
       (diredp-compressed-file-suffix ((t (:foreground "red"))))
       (diredp-date-time ((t (:foreground "purple"))))
       (diredp-dir-heading ((t ,directory)))
       (diredp-dir-priv ((t ,directory)))
       (diredp-exec-priv ((t (:background "green"))))
       (diredp-executable-tag ((t (:foreground "green3" :background "white"))))
       (diredp-file-name ((t (:foreground "black"))))
       (diredp-file-suffix ((t (:foreground "#6DB604"))))
       (diredp-flag-mark-line ((t ,marked-line)))
       (diredp-ignored-file-name ((t (:strike-through t :foreground "red"))))
       (diredp-read-priv ((t (:foreground "white" :background "blue"))))
       (diredp-write-priv ((t (:foreground "white" :background "red"))))

       ;; flypell
       (flyspell-duplicate-face ((t (:weight bold :underline t :foreground "gold3"))))
       (flyspell-incorrect-face ((t (:weight bold :underline t :foreground "red"))))

       ;; LaTeX
       (font-latex-bold-face ((t (:weight bold :foreground "medium sea green"))))
       (font-latex-math-face ((t (:foreground "blue1"))))
       (font-latex-sectioning-1-face ((t (:family "Arial" :height 2.7 :weight bold :foreground "cornflower blue"))))
       (font-latex-sectioning-2-face ((t ,ol1)))
       (font-latex-sectioning-3-face ((t ,ol2)))
       (font-latex-sectioning-4-face ((t ,ol3)))
       (font-latex-sectioning-5-face ((t ,ol4)))
       (font-latex-sedate-face ((t (:foreground "gray45"))))
       (font-latex-verbatim-face ((t (:foreground "tan1"))))

       ;; font-lock
       (font-lock-builtin-face ((t (:foreground "orchid"))))
       (font-lock-comment-delimiter-face ((t (:foreground "red"))))
       (font-lock-comment-face ((t (:slant italic :foreground "red"))))
       (font-lock-constant-face ((t (:foreground "dark cyan"))))
       (font-lock-doc-face ((t (:foreground "#EB4264"))))
       (font-lock-function-name-face ((t (:foreground "blue"))))
       (font-lock-keyword-face ((t (:foreground "purple1"))))
       (font-lock-reference-face ((t (:foreground "dark cyan"))))
       (font-lock-string-face ((t (:foreground "#3AA221"))))
       (font-lock-type-face ((t (:foreground "forest green"))))
       (font-lock-variable-name-face ((t (:foreground "magenta"))))
       (font-lock-warning-face ((t (:weight bold :foreground "red"))))

       (fringe ((t (:foreground "#AFB7BA" :background "#DDEEFF"))))

       ;; Gnus
       (gnus-cite-attribution-face ((t (:foreground "#5050B0"))))
       (gnus-cite-face-1 ((t (:foreground "#5050B0"))))
       (gnus-cite-face-2 ((t (:foreground "#660066"))))
       (gnus-cite-face-3 ((t (:foreground "#007777"))))
       (gnus-cite-face-4 ((t (:foreground "#990000"))))
       (gnus-cite-face-5 ((t (:foreground "#000099"))))
       (gnus-cite-face-6 ((t (:foreground "#BB6600"))))
       (gnus-cite-face-7 ((t (:foreground "#5050B0"))))
       (gnus-cite-face-8 ((t (:foreground "#660066"))))
       (gnus-cite-face-9 ((t (:foreground "#007777"))))
       (gnus-cite-face-10 ((t (:foreground "#990000"))))
       (gnus-emphasis-bold ((t (:weight bold))))
       (gnus-emphasis-highlight-words ((t (:foreground "yellow" :background "black"))))
       (gnus-group-mail-1-empty-face ((t (:foreground "#5050B0"))))
       (gnus-group-mail-1-face ((t (:weight bold :foreground "#FF50B0"))))
       (gnus-group-mail-2-empty-face ((t (:foreground "#660066"))))
       (gnus-group-mail-2-face ((t (:weight bold :foreground "#FF0066"))))
       (gnus-group-mail-3-empty-face ((t (:foreground "#007777"))))
       (gnus-group-mail-3-face ((t (:weight bold :foreground "#FF7777"))))
       (gnus-group-news-1-empty-face ((t (:foreground "#5050B0"))))
       (gnus-group-news-1-face ((t (:weight bold :foreground "#FF50B0"))))
       (gnus-group-news-2-empty-face ((t (:foreground "#660066"))))
       (gnus-group-news-2-face ((t (:weight bold :foreground "#FF0066"))))
       (gnus-group-news-3-empty-face ((t (:foreground "#007777"))))
       (gnus-group-news-3-face ((t (:weight bold :foreground "#FF7777"))))
       (gnus-group-news-4-empty-face ((t (:foreground "#990000"))))
       (gnus-group-news-4-face ((t (:weight bold :foreground "#FF0000"))))
       (gnus-group-news-5-empty-face ((t (:foreground "#000099"))))
       (gnus-group-news-5-face ((t (:weight bold :foreground "#FF0099"))))
       (gnus-group-news-6-empty-face ((t (:foreground "#BB6600"))))
       (gnus-group-news-6-face ((t (:weight bold :foreground "#FF6600"))))
       (gnus-header-content-face ((t (:family "Arial" :foreground "#3399CC"))))
       (gnus-header-from-face ((t (:family "Arial" :foreground "blue"))))
       (gnus-header-name-face ((t (:family "Arial" :weight bold :foreground "#3399CC"))))
       (gnus-header-newsgroups-face ((t (:family "Arial" :foreground "#3399CC"))))
       (gnus-header-subject-face ((t (:weight bold :foreground "#FF6633"))))
       (gnus-picon-face ((t (:foreground "yellow" :background "white"))))
       (gnus-picon-xbm-face ((t (:foreground "yellow" :background "white"))))
       (gnus-signature-face ((t (:foreground "#7F7F7F"))))
       (gnus-splash-face ((t (:foreground "gold"))))
       (gnus-summary-cancelled-face ((t (:slant italic :strike-through t :foreground "gray55"))))
       (gnus-summary-high-ancient-face ((t (:weight normal :foreground "#1A44B6" :background "#E0E0E0"))))
       (gnus-summary-high-read-face ((t (:weight normal :foreground "#1A44B6" :background "#E0E0E0"))))
       (gnus-summary-high-ticked-face ((t (:weight normal :foreground "#009900"))))
       (gnus-summary-high-unread-face ((t (:weight normal :foreground "#1A44B6"))))
       (gnus-summary-low-ancient-face ((t (:slant italic :foreground "gray50" :background "#E0E0E0"))))
       (gnus-summary-low-read-face ((t (:slant italic :foreground "gray50" :background "#E0E0E0"))))
       (gnus-summary-low-ticked-face ((t (:slant italic :foreground "#009900"))))
       (gnus-summary-low-unread-face ((t (:slant italic :foreground "gray50"))))
       (gnus-summary-normal-ancient-face ((t (:foreground "black" :background "#E0E0E0"))))
       (gnus-summary-normal-read-face ((t (:foreground "black" :background "#E0E0E0"))))
       (gnus-summary-normal-ticked-face ((t (:foreground "#009900"))))
       (gnus-summary-normal-unread-face ((t (:foreground "black"))))
       (gnus-summary-selected-face ((t (:background "#FFD0D0"))))
       (gnus-x-face ((t (:foreground "black" :background "white"))))

       (highlight ((t (:background "#FFFFA0"))))  ; used by hlt package of Drew Adams
       (highlight-symbol-face ((t (:background "#FFFFA0"))))
       (hl-line ((t (:background "cornsilk3"))))  ; #E6FEC0 or #D4EAFF

       (html-helper-bold-face ((t (:weight bold :foreground "black"))))
       (html-helper-italic-face ((t (:slant italic :foreground "black"))))
       (html-helper-underline-face ((t (:underline t :foreground "black"))))
       (html-tag-face ((t (:foreground "blue"))))

       ;; Info / info+
       (info-file ((t (:family "Arial" :height 1.8 :weight bold :box (:line-width 1 :color "#0000CC") :foreground "cornflower blue" :background "LightSteelBlue1")))) 
       (info-header-node ((t (:underline t :foreground "orange"))))  ; nodes in header
       (info-header-xref ((t (:underline t :foreground "dodger blue"))))  ; cross references in header
       (info-menu-header ((t (:family "Arial" :height 1.6 :weight bold :underline t :foreground "#00CC00"))))  ; menu titles (headers) -- major topics
       (info-menu-star ((t (:foreground "black"))))  ; every 3rd menu item
       (info-node ((t (:underline t :foreground "blue"))))  ; node names
       (info-quoted-name ((t ,inline-code)))
       (info-string ((t (:foreground "green4"))))  ; strings ("...")
       (info-title-1-face ((t (:family "Arial" :height 1.6 :weight bold :box (:line-width 1 :color "#00CC00") :foreground "lime green" :background "DarkSeaGreen1"))))
       (info-title-2-face ((t (:family "Arial" :height 1.3 :weight bold :foreground "#F9A049"))))
       (info-title-3-face ((t (:family "Arial" :height 1.2 :weight bold :foreground "red3"))))
       (info-xref ((t (:weight bold :underline t :foreground "blue"))))  ; unvisited cross-references
       (info-xref-visited ((t (:weight bold :foreground "magenta4"))))  ; previously visited cross-references

       ;; highlighting matches
       (isearch ((t (:weight bold :foreground "#EE6600" :background "yellow"))))
       (isearch-lazy-highlight-face ((t (:weight bold :foreground "blue" :background "#CCFFFF"))))  ; for GNU Emacs
       (isearch-secondary ((t (:weight bold :foreground "blue" :background "#CCFFFF"))))  ; for XEmacs

       (light-symbol-face ((t (:background "#FFFFA0"))))

       (linum ((t (:foreground "#AFB7BA" :background "#DDEEFF"))))

       ;; IRC
       (lui-button-face ((t (:underline t :foreground "blue"))))
       (lui-highlight-face ((t (:foreground "red"))))  ; my nick
       (lui-time-stamp-face ((t (:foreground "orange"))))

       ;; (magit-branch ((t )))
       (magit-diff-add ((t (:inherit diff-added))))
       (magit-diff-del ((t (:inherit diff-removed))))
       ;; (magit-diff-file-header ((t )))
       ;; (magit-diff-hunk-header ((t )))
       ;; (magit-diff-none ((t )))
       ;; (magit-header ((t )))
       ;; (magit-item-highlight ((t )))
       ;; (magit-item-mark ((t )))
       ;; (magit-log-head-label ((t )))
       ;; (magit-log-tag-label ((t )))
       ;; (magit-section-title ((t )))

       ;; make
       (makefile-space-face ((t (:background "hot pink"))))
       (makefile-targets ((t (:weight bold :foreground "blue"))))

       (match ((t (:weight bold :foreground "#EE6600" :background "yellow"))))

       ;; Message
       (message-cited-text-face ((t (:foreground "#5050B0"))))
       (message-header-cc-face ((t (:family "Arial" :foreground "blue"))))
       (message-header-name-face ((t (:family "Arial" :weight bold :foreground "#3399CC"))))
       (message-header-newsgroups-face ((t (:family "Arial" :foreground "#3399CC"))))
       (message-header-other-face ((t (:family "Arial" :foreground "#3399CC"))))
       (message-header-subject-face ((t (:weight bold :foreground "#FF6633"))))
       (message-header-to-face ((t (:family "Arial" :foreground "blue"))))
       (message-header-xheader-face ((t (:foreground "red"))))
       (message-mml-face ((t (:foreground "forest green"))))
       (message-separator-face ((t (:family "Arial" :weight bold :foreground "red"))))

       (minibuffer-prompt ((t (:weight bold :foreground "#FF6633"))))

       ;; GNU Emacs mode-line
       (mode-line ((t (:box (:line-width 1 :color "#9EABB6") :foreground "#FFC77F" :background "#0097CB"))))
       (mode-line-buffer-id ((t (:weight bold :foreground "white" :background "#0097CB"))))
       (mode-line-highlight ((t (:foreground "yellow"))))
       (mode-line-inactive ((t (:box (:line-width 1 :color "#9EABB6") :foreground "#617A82" :background "#006382"))))

       ;; XEmacs modeline
       (modeline-mousable ((t (:foreground "firebrick"))))  ; major-mode string
       (modeline-mousable-minor-mode ((t (:foreground "green4"))))

       ;; non-breaking space
       (nobreak-space ((t (:background "#C6C3C6"))))

       (nxml-attribute-local-name-face ((t (:foreground "magenta"))))
       (nxml-attribute-value-delimiter-face ((t (:foreground "green4"))))
       (nxml-attribute-value-face ((t (:foreground "green4"))))
       (nxml-comment-content-face  ((t (:slant italic :foreground "red"))))
       (nxml-comment-delimiter-face ((t (:foreground "red"))))
       (nxml-element-local-name-face ((t (:foreground "blue"))))
       (nxml-processing-instruction-delimiter-face ((t (:foreground "purple1"))))
       (nxml-processing-instruction-target-face ((t (:foreground "purple1"))))
       (nxml-tag-delimiter-face ((t (:foreground "blue"))))
       (nxml-tag-slash-face ((t (:foreground "blue"))))

       ;; Org
       (org-agenda-clocking ((t ,clock-line)))
       (org-agenda-column-dateline ((t (:inherit org-column))))
       ;; (org-agenda-column-dateline ((t (:background "deep sky blue" :height 79 :family "Consolas"))))
       (org-agenda-date ((t (:height 1.6 :weight bold :foreground "#0063F5")))) ; "87C9FC"
       ;; (org-agenda-date ((t (:foreground "blue1"))))
       (org-agenda-date-today ((t (:foreground "purple"))))  ; "#CCCCFF"  ; inherit
       ;; (org-agenda-date-today ((t (:slant italic :weight bold :foreground "blue1"))))
       (org-agenda-date-weekend ((t (:foreground "dim gray"))))  ; "#B6B2AE"  ; inherit
       ;; (org-agenda-date-weekend ((t (:weight bold :foreground "blue1"))))
       (org-agenda-diary ((t (:weight bold :foreground "green4" :background "light blue"))))
       (org-agenda-dimmed-todo-face ((t (:foreground "gold2"))))  ; org-blocked-todo
       (org-agenda-done ((t (:foreground "gray50" :background "#EAFFEA"))))
       (org-agenda-restriction-lock ((t (:foreground "white" :background "SkyBlue4"))))
       (org-agenda-structure ((t (:foreground "#008ED1"))))
       (org-archived ((t (:foreground "gray70"))))
       (org-beamer-tag ((t (:box (:line-width 1 :color "#00CC00" :style nil)))))
       (org-block ((t ,block-code)))
       (org-checkbox ((t (:weight bold :background "gray"))))
       (org-clock-overlay ((t (:foreground "white" :background "SkyBlue4"))))
       (org-code ((t ,inline-code)))
       (org-column ((t ,column)))
       ;; (org-column ((t (:background "gold" :height 79 :family "Consolas"))))
       (org-column-title ((t (:inherit org-column))))
       ;; (org-column-title ((t (:background "gold" :height 79 :family "Consolas"))))
       (org-date ((t (:underline t :foreground "purple"))))
       (org-dim ((t (:foreground "#AAAAAA"))))
       (org-document-info ((t (:foreground "midnight blue"))))
       (org-document-info-keyword ((t (:foreground "#008ED1" :background "#EAEAFF"))))
       (org-document-title ((t (:family "Arial" :height 1.8 :weight bold :foreground "blue"))))
       (org-done ((t (:weight bold :box (:line-width 1 :color "black") :foreground "gray31"))))
       ;; (org-done ((t (:foreground "green3" :weight bold :strike-through t))))
       (org-drawer ((t (:foreground "light sky blue"))))
       (org-ellipsis ((t (:foreground "white"))))
       (org-footnote ((t (:underline t :foreground "#008ED1"))))
       (org-formula ((t (:foreground "chocolate1"))))
       (org-headline-done ((t (:family "Arial" :height 1.6 :weight bold :strike-through t :box (:line-width 1 :color "gray67") :foreground "gray67" :background "gray92"))))
       (org-latex-and-export-specials ((t (:foreground "blue1"))))
       (org-level-1 ((t ,ol1)))
       (org-level-2 ((t ,ol2)))
       (org-level-3 ((t ,ol3)))
       (org-level-4 ((t ,ol4)))
       (org-level-5 ((t ,ol5)))
       (org-level-6 ((t ,ol6)))
       (org-level-7 ((t ,ol7)))
       (org-level-8 ((t ,ol8)))
       (org-meta-line ((t (:foreground "#008ED1" :background "#EAEAFF"))))
       (org-mode-line-clock ((t ,clock-line)))
       (org-mode-line-clock-overrun ((t (:weight bold :box (:line-width 1 :color "#9EABB6") :foreground "white" :background "#FF4040"))))
       (org-property-value ((t (nil))))
       (org-scheduled ((t (:foreground "blue"))))
       (org-scheduled-previously ((t (:foreground "red"))))
       (org-scheduled-today ((t (:foreground "#00BB00"))))
       (org-sexp-date ((t (:foreground "purple"))))
       (org-special-keyword ((t (:foreground "#00BB00" :background "#EAFFEA"))))
       (org-table ((t (:foreground "blue1" :background "#EAFFEA"))))
       (org-tag ((t (:height 1.0 :weight bold :slant italic :foreground "#816A7D" :background "#F7B8DE"))))
       (org-target ((t (:underline t))))
       (org-time-grid ((t (:foreground "#008ED1"))))
       (org-todo ((t (:weight bold :box (:line-width 1 :color "red3") :foreground "white" :background "#FF5555"))))
       (org-upcoming-deadline ((t (:foreground "#FF5555"))))
       ;; (org-upcoming-deadline ((t (:foreground "white" :background "#E9A36A" :weight bold))))
       (org-verbatim ((t ,inline-code)))
       (org-warning ((t (:weight bold :foreground "#D80000" :background "white"))))

       ;; Org non-standard faces
       (my/org-deadline-later ((t (:foreground "black" :background "#AACAFC"))))
       (my/org-deadline-today ((t (:weight bold :foreground "white" :background "#E9A36A"))))
       (my/org-deadline-tomorrow ((t (:weight bold :foreground "black" :background "#B4F1B3"))))
       (my/org-deadline-yesterday ((t (:weight bold :foreground "white" :background "#D80000"))))

       (my/org-started-kwd-face ((t (:weight bold :box (:line-width 1 :color "orange red") :foreground "black" :background "#FF9000"))))
       (my/org-waiting-kwd-face ((t (:weight bold :box (:line-width 1 :color "#00CC00") :foreground "black" :background "chartreuse"))))
       (my/org-delegated-kwd-face ((t (:weight bold :box (:line-width 1 :color "#00CC00") :foreground "black" :background "chartreuse"))))
       (my/org-deferred-kwd-face ((t (:weight bold :box (:line-width 1 :color "steel blue") :foreground "black" :background "cyan"))))
       (my/org-someday-kwd-face ((t (:weight bold :box (:line-width 1 :color "steel blue") :foreground "black" :background "cyan"))))
       (my/org-canceled-kwd-face ((t (:weight bold :box (:line-width 1 :color "#D80000") :foreground "red" :background "cyan"))))

       (my/org-quote-kwd-face ((t (:weight bold :box (:line-width 1 :color "red3") :foreground "white" :background "#FF5555"))))
       (my/org-quoted-kwd-face ((t (:weight bold :box (:line-width 1 :color "red3") :foreground "black" :background "#FF9000"))))
       (my/org-approved-kwd-face ((t (:weight bold :box (:line-width 1 :color "red3") :foreground "black" :background "chartreuse"))))
       (my/org-rejected-kwd-face ((t (:weight bold :box (:line-width 1 :color "red3") :foreground "red" :background "cyan"))))
       (my/org-expired-kwd-face ((t (:weight bold :box (:line-width 1 :color "red3") :foreground "white" :background "gray31"))))

       (my/org-openpo-kwd-face ((t (:weight bold :box (:line-width 1 :color "orange red") :foreground "white" :background "#FF5555"))))
       (my/org-closedpo-kwd-face ((t (:weight bold :box (:line-width 1 :color "orange red") :foreground "white" :background "gray31"))))

       ;; pabbrev
       (pabbrev-debug-display-label-face ((t (:background "chartreuse"))))
       (pabbrev-suggestions-label-face ((t (:weight bold :foreground "white" :background "purple"))))
       (pabbrev-suggestions-face ((t (:weight bold :foreground "white" :background "red"))))

       ;; parentheses
       (paren-face-match ((t (:background "chartreuse"))))
       (paren-face-mismatch ((t (:weight bold :foreground "white" :background "purple"))))
       (paren-face-no-match ((t (:weight bold :foreground "white" :background "purple"))))

       (pesche-space ((t (:background "lemon chiffon"))))
       (pesche-tab ((t (:background "gold"))))

       ;; pretty print ^L
       (pp^L-highlight ((t (:strike-through t :inherit shadow))))

       (recover-this-file ((t (:foreground "white" :background "red"))))

       ;; selection, selected region
       (region ((t (:background "#D2D9E0"))))  ; for GNU Emacs ; was seashell2
       (zmacs-region ((t (:background "gray84"))))  ; for XEmacs

       (secondary-selection ((t (:foreground "purple" :background "yellow"))))  ; used by Org-mode for highlighting matched entries and keywords

       ;; for `cat <<EOF' in shell scripts
       (sh-heredoc ((t (:foreground "blue1"))))

       ;; shell
       (shell-option-face ((t (:foreground "forest green"))))
       (shell-output-2-face ((t (:foreground "blue"))))
       (shell-output-3-face ((t (:foreground "purple"))))
       (shell-output-face ((t (:foreground "black"))))
       (shell-prompt-face ((t (:weight bold :foreground "yellow"))))

       ;; parentheses
       (show-paren-match-face ((t (:background "chartreuse"))))
       (show-paren-mismatch-face ((t (:weight bold :foreground "white" :background "purple"))))

       ;; speedbar
       (speedbar-button-face ((t (:foreground "green4"))))
       (speedbar-directory-face ((t (:foreground "blue4"))))
       (speedbar-file-face ((t (:foreground "cyan4"))))
       (speedbar-highlight-face ((t (:background "green"))))
       (speedbar-selected-face ((t (:underline t :foreground "red"))))
       (speedbar-tag-face ((t (:foreground "brown"))))

       ;; subversion
       (svn-status-directory-face ((t ,directory)))
       (svn-status-filename-face ((t (:foreground "magenta3"))))
       (svn-status-locked-face ((t (:weight bold :foreground "red"))))
       (svn-status-marked-face ((t ,marked-line)))
       (svn-status-marked-popup-face ((t (:weight bold :foreground "green3"))))
       (svn-status-switched-face ((t (:slant italic :foreground "gray55"))))
       (svn-status-symlink-face  ((t ,symlink)))
       (svn-status-update-available-face ((t (:foreground "orange"))))

       ;; TeX
       (tex-verbatim ((t (:foreground "blue1"))))

       ;; tool-bar
       (tool-bar ((t (:box (:line-width 1 :style released-button) :foreground "black" :background "gray75"))))

       ;; tooltip
       (tooltip ((t (:foreground "black" :background "light yellow"))))

       ;; show trailing whitespace
       (trailing-whitespace ((t (:background "#D5FFD5"))))

       (traverse-match-face ((t (:weight bold :foreground "blue violet"))))

       ;; w3m
       (w3m-anchor ((t (:foreground "blue"))))
       (w3m-arrived-anchor ((t (:foreground "purple1"))))
       (w3m-bitmap-image-face ((t (:foreground "gray4" :background "green"))))
       (w3m-bold ((t (:weight bold :foreground "medium sea green"))))
       (w3m-current-anchor ((t (:weight bold :underline t :foreground "blue"))))
       (w3m-form ((t (:underline t :foreground "tan1"))))
       (w3m-form-button-face ((t (:weight bold :underline t :foreground "gray4" :background "light grey"))))
       ;; (w3m-form-button-face ((t (:background "lightgray" :foreground "black"))))
       (w3m-form-button-mouse-face ((t (:underline t :foreground "light grey" :background "#2B7E2A"))))
       ;; (w3m-form-button-mouse-face ((t (:background "orange"))))
       (w3m-form-button-pressed-face ((t (:weight bold :underline t :foreground "gray4" :background "light grey"))))
       ;; (w3m-form-button-pressed-face ((t (:background "yellow"))))
       (w3m-header-line-location-content-face ((t (:foreground "purple2"))))
       (w3m-header-line-location-title-face ((t (:foreground "cadet blue"))))
       (w3m-history-current-url-face ((t (:foreground "lemon chiffon"))))
       ;; (w3m-history-current-url-face ((t (:foreground "LightSkyBlue" :background "SkyBlue4"))))
       (w3m-image-face ((t (:weight bold :foreground "DarkSeaGreen2"))))
       (w3m-link-numbering ((t (:foreground "#B4C7EB"))))     ;; mouseless browsing
       (w3m-strike-through-face ((t (:strike-through t))))
       (w3m-underline-face ((t (:underline t))))
       ;; (w3m-tab-background-face ((t (:foreground "white" :background "#21364B"))))
       ;; (w3m-tab-selected-face ((t (:foreground "black" :background "Gray85" :box (:line-width 1 :style nil)))))
       ;; (w3m-tab-selected-retrieving-face ((t (:background "gray85" :foreground "white" :box (:line-width -1 :style nil)))))
       ;; (w3m-tab-unselected-face ((t (:foreground "gray20" :background "gray70" :box (:line-width 1 :style nil)))))
       ;; (w3m-tab-unselected-retrieving-face ((t (:foreground "white" :background "gray50" :box (:line-width -1 :style nil)))))

       (widget-button-face ((t (:weight bold))))
       (widget-button-pressed-face ((t (:foreground "red"))))
       (widget-documentation-face ((t (:foreground "green4"))))
       (widget-field-face ((t (:background "gray85"))))
       (widget-inactive-face ((t (:foreground "dim gray"))))
       (widget-single-line-field-face ((t (:background "gray85"))))
       ))))

(add-to-list 'color-themes
             '(color-theme-sva "Sva" "Sebastien Vauban"))

(provide 'color-theme-sva)


;; This is for the sake of Emacs.
;; Local Variables:
;; ispell-local-dictionary: "en_US"
;; mode: outline-minor
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode))
;; End:

;;; color-theme-sva.el ends here
