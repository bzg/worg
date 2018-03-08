;;; zenash.el --- a low-saturation, light-background version of zenburn.
;; Author: Yavuz Arkun

;; Original Author of zenburn.el: Daniel Brockman <daniel@brockman.se>
;; 
;; 

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Some packages ship with broken implementations of `format-spec';
;; for example, stable versions of TRAMP and ERC do this.  To fix
;; this, you can put the following at the end of your ~/.emacs:

;;   (unless (zenash-format-spec-works-p)
;;     (zenash-define-format-spec))

;; Thanks to Jani Nurminen, who created the original zenburn color
;; theme for vim.  I'm just copying him. :-)

;;; Short-Term Wishlist:

;; Theme the ansi-term faces `term-red', etc., and the ERC faces
;; `fg:erc-color-face1', etc.

;; Theme `gnus-server-offline-face', `gnus-server-opened-face', and
;; `gnus-server-denied-face'.  First, find out what they hell they do.

;; Theme `gnus-emphasis-highlight-words' after finding out what it
;; does.

;; Theme `emms-stream-name-face' and `emms-stream-url-face'.

;; Theme `ido-indicator-face'.

;;; Code:

(require 'color-theme)

(defvar zenash-fg            "#4F4C48")
(defvar zenash-fg+1          "#363431")
(defvar zenash-fg+2          "#262421")

(defvar zenash-bg-3          "#D1CDC5")
(defvar zenash-bg-2          "#C9C5BD")
(defvar zenash-bg-1          "#C1BDB5")
(defvar zenash-bg            "#B0ACA5")
(defvar zenash-bg+1          "#A39F99")
(defvar zenash-bg+2          "#918C84")

(defvar zenash-selection-bg  "#858A8C")
(defvar zenash-selection-fg  "#E6E5E0")

(defvar zenash-dark-gray     "#575753")
(defvar zenash-light-blue    "#5181B4")
(defvar zenash-light-cyan    "#6B9898")
(defvar zenash-light-green   "#7E9B62")
(defvar zenash-light-magenta "#895E84")
(defvar zenash-light-red     "#CB5151")
(defvar zenash-light-yellow  "#C1A128")
(defvar zenash-white         "#F0EEE4")
(defvar zenash-black         "#000000")
(defvar zenash-dark-blue     "#1D5193")
(defvar zenash-dark-cyan     "#338F96")
(defvar zenash-dark-green    "#4D6B31")
(defvar zenash-dark-magenta  "#56355C")
(defvar zenash-dark-red      "#993D3D")
(defvar zenash-dark-yellow   "#C08B18")
(defvar zenash-light-gray    "#ADB3B5")

(defvar zenash-ll-1          "#606060")
(defvar zenash-ll-2          "#708070")

(defvar zenash-yellow        zenash-dark-yellow)
(defvar zenash-yellow-1      "#e0cf9f")
(defvar zenash-yellow-2      "#C4A244")
(defvar zenash-yellow-3      zenash-light-yellow)

(defvar zenash-orange        "#D38226")
(defvar zenash-brown         "#6B3722")

(defvar zenash-red+1         "#dca3a3")
(defvar zenash-red           zenash-dark-red)
(defvar zenash-red-1         "#8C5D5D")
(defvar zenash-red-2         "#BF7575")
(defvar zenash-red-3         "#9c6363")
(defvar zenash-red-4         zenash-light-red)

(defvar zenash-green-1       "#457C7D")
(defvar zenash-green         zenash-dark-green)
(defvar zenash-green+1       "#457C7D")
(defvar zenash-green+2       "#457C7D")
(defvar zenash-green+3       "#457C7D")
(defvar zenash-green+4       zenash-light-green)

(defvar zenash-blue+1        "#94bff3")
(defvar zenash-blue          zenash-dark-blue)
(defvar zenash-blue-1        "#3E80B4")
(defvar zenash-blue-2        "#547093")
(defvar zenash-blue-3        "#718093")
(defvar zenash-blue-4        zenash-light-blue)

(defvar zenash-cyan          zenash-dark-cyan)

(defvar font-lock-pseudo-keyword-face 'font-lock-pseudo-keyword-face)
(defvar font-lock-operator-face 'font-lock-operator-face)

(defun zenash-format-spec-works-p ()
  (and (fboundp 'format-spec)
       (= (next-property-change
           0 (format-spec #("<%x>" 0 4 (face (:weight bold)))
                          '((?x . "foo"))) 4) 4)))

(defun zenash-format-spec (format specification)
  "Return a string based on FORMAT and SPECIFICATION.
FORMAT is a string containing `format'-like specs like \"bash %u %k\",
while SPECIFICATION is an alist mapping from format spec characters
to values."
  (with-temp-buffer
    (insert format)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (cond
       ;; Quoted percent sign.
       ((eq (char-after) ?%)
        (delete-char 1))
       ;; Valid format spec.
       ((looking-at "\\([-0-9.]*\\)\\([a-zA-Z]\\)")
        (let* ((num (match-string 1))
               (spec (string-to-char (match-string 2)))
               (val (cdr (assq spec specification))))
          (unless val
            (error "Invalid format character: %s" spec))
          (let ((text (format (concat "%" num "s") val)))
            (insert-and-inherit text)
            ;; Delete the specifier body.
            (delete-region (+ (match-beginning 0) (length text))
                           (+ (match-end 0) (length text)))
            ;; Delete the percent sign.
            (delete-region (1- (match-beginning 0)) (match-beginning 0)))))
       ;; Signal an error on bogus format strings.
       (t
        (error "Invalid format string"))))
    (buffer-string)))

(defun zenash-define-format-spec ()
  (interactive)
  (fset 'format-spec #'zenash-format-spec))

(unless (zenash-format-spec-works-p)
  (zenash-define-format-spec))

(eval-after-load 'format-spec
  (unless (zenash-format-spec-works-p)
    (zenash-define-format-spec)))

(setq-default mode-line-buffer-identification
              (list (propertize "%12b" 'face
                                (list :weight 'bold
                                      :foreground zenash-bg-3))))
(setq-default mode-line-frame-identification "")
(setq-default erc-mode-line-format
              (concat (propertize "%t" 'face
                                  (list :weight 'bold
                                        :foreground zenash-bg-3))
                      " %a"))

(setq gnus-logo-colors `(,zenash-bg+2 ,zenash-bg+1)
      gnus-mode-line-image-cache
      '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    11        2            1\",
/* colors */
\". c #dcdccc\",
\"# c None s None\",
/* pixels */
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\"};"))

(defun zenash-make-face-alias-clauses (alias-symbols)
  (let (clauses)
    (dolist (alias-symbol alias-symbols clauses)
      (let ((alias-name (symbol-name alias-symbol)))
        (if (not (string-match "-face" alias-name))
            (error "Invalid face alias: %s" alias-name)
          (let ((target-name (replace-regexp-in-string
                              ".*\\(-face\\)" ""
                              alias-name nil nil 1)))
            (push `(,(intern alias-name)
                    ((t (:inherit ,(intern target-name)))))
                  clauses)))))))

;;;###autoload
(defun color-theme-zenash ()
  "Just some alien fruit salad to keep you in the zone."
  (interactive)
  (color-theme-install
   (append
    (list 'color-theme-zenash
          
     `((background-color . ,zenash-bg)
       (background-mode . light)
       (border-color . ,zenash-bg)
       (foreground-color . ,zenash-fg)
       (mouse-color . ,zenash-fg))
      
     `((emms-mode-line-icon-color . ,zenash-fg)
       (goto-address-mail-face . italic)
       (goto-address-mail-mouse-face . secondary-selection)
       (goto-address-url-face . bold)
       (goto-address-url-mouse-face . hover-highlight)
       (help-highlight-face . hover-highlight)
       (imaxima-label-color . ,zenash-yellow-2)
       (imaxima-equation-color . ,zenash-fg)
       (list-matching-lines-face . bold)
       (view-highlight-face . hover-highlight)
       (widget-mouse-face . hover-highlight))

     `(bold                            ((t (:weight bold))))
     `(bold-italic                     ((t (:italic t :weight bold))))
     `(default                         ((t (:background ,zenash-bg :foreground ,zenash-fg))))
     `(fixed-pitch                     ((t (:weight bold))))
     `(italic                          ((t (:slant italic))))
     `(underline                       ((t (:underline t))))

     `(zenash-foreground+1             ((t (:foreground ,zenash-fg+1))))
     `(zenash-foreground+2             ((t (:foreground ,zenash-fg+2))))

     `(zenash-background-1             ((t (:background ,zenash-bg-1))))
     `(zenash-background-2             ((t (:background ,zenash-bg-2))))
     `(zenash-background+1             ((t (:background ,zenash-bg+1))))
     `(zenash-background+2             ((t (:background ,zenash-bg+2))))
     
     `(zenash-primary-1                ((t (:foreground ,zenash-blue :weight bold))))
     `(zenash-primary-2                ((t (:foreground ,zenash-red :weight bold))))
     `(zenash-primary-3                ((t (:foreground ,zenash-green :weight bold))))
     `(zenash-primary-4                ((t (:foreground ,zenash-yellow :weight bold))))
     `(zenash-primary-5                ((t (:foreground ,zenash-dark-magenta :weight bold))))

     `(zenash-highlight-damp           ((t (:foreground ,zenash-ll-1 :background ,zenash-bg-1))))
     `(zenash-highlight-alerting       ((t (:background ,zenash-bg+2))))
     `(zenash-highlight-subtle         ((t (:background ,zenash-bg+1))))

     `(zenash-lowlight-1               ((t (:foreground ,zenash-ll-1))))
     `(zenash-lowlight-2               ((t (:foreground ,zenash-ll-2))))

     `(zenash-yellow-2                 ((t (:foreground ,zenash-yellow-2))))
     `(zenash-orange                   ((t (:foreground ,zenash-orange))))
     `(zenash-red                      ((t (:foreground ,zenash-red))))
     `(zenash-green-1                  ((t (:foreground ,zenash-green-1))))
     `(zenash-green                    ((t (:foreground ,zenash-green))))
     `(zenash-green+1                  ((t (:foreground ,zenash-green+1))))
     `(zenash-green+2                  ((t (:foreground ,zenash-green+2))))
     `(zenash-green+3                  ((t (:foreground ,zenash-green+3))))
     `(zenash-green+4                  ((t (:foreground ,zenash-green+4))))
     `(zenash-blue                     ((t (:foreground ,zenash-blue))))
     `(zenash-blue-1                   ((t (:foreground ,zenash-blue-1))))
     `(zenash-blue-2                   ((t (:foreground ,zenash-blue-2))))
     `(zenash-blue-3                   ((t (:foreground ,zenash-blue-3))))
     `(zenash-blue-4                   ((t (:foreground ,zenash-blue-4))))

     `(zenash-title                    ((t (:inherit variable-pitch :weight bold))))

     `(font-lock-builtin               ((t (:inherit zenash-foreground+1))))
     `(font-lock-comment               ((t (:inherit zenash-lowlight-2 :slant italic))))
     `(font-lock-comment-delimiter     ((t (:inherit zenash-lowlight-2 :slant italic))))
     `(font-lock-constant              ((t (:inherit zenash-red-1 :slant italic))))
     `(font-lock-doc                   ((t (:inherit zenash-lowlight-1))))
     `(font-lock-function-name         ((t (:inherit zenash-blue-2 :slant italic))))
     `(font-lock-keyword               ((t (:inherit zenash-foreground+1 :weight bold))))
     `(font-lock-negation-char         ((t (:inherit zenash-primary-1))))
     `(font-lock-preprocessor          ((t (:inherit zenash-blue-1))))
     `(font-lock-string                ((t (:foreground ,zenash-red-1))))
     `(font-lock-type                  ((t (:inherit zenash-green))))
     `(font-lock-variable-name         ((t (:foreground ,zenash-blue-2))))
     `(font-lock-warning               ((t (:inherit zenash-highlight-alerting))))

     `(font-lock-pseudo-keyword        ((t (:inherit zenash-primary-2))))
     `(font-lock-operator              ((t (:inherit zenash-primary-3))))

     `(border                          ((t (:background ,zenash-bg))))
     `(fringe                          ((t (:inherit zenash-highlight-subtle))))
     `(header-line                     ((t (:foreground ,zenash-fg :background ,zenash-bg+2))))
     `(mode-line                       ((t (:foreground ,zenash-fg :background ,zenash-bg+2))))
     `(mode-line-inactive              ((t (:foreground ,zenash-ll-2 :background ,zenash-bg+1))))
     `(mode-line-buffer-id             ((t (:foreground ,zenash-bg-2))))
     `(mode-line-highlight             ((t (:background ,zenash-bg-2))))
     `(mode-line-emphasis              ((t (:weight bold))))
     `(minibuffer-prompt               ((t (:inherit zenash-primary-1))))
     `(Buffer-menu-buffer              ((t (:weight bold))))

     `(region                          ((t (:foreground ,zenash-selection-fg :background ,zenash-selection-bg))))
     `(secondary-selection             ((t (:foreground ,zenash-fg :background ,zenash-bg+2))))

     `(trailing-whitespace             ((t (:inherit font-lock-warning))))
     `(highlight                       ((t (:inherit zenash-background-1))))
     `(highlight-current-line          ((t (:inherit zenash-background+2))))
     `(paren                           ((t (:inherit zenash-lowlight-1))))
     `(show-paren-mismatch             ((t (:inherit font-lock-warning))))
     `(show-paren-match                ((t (:foreground ,zenash-selection-fg :background ,zenash-selection-bg))))
     `(match                           ((t (:weight bold))))

     `(cursor                          ((t (:background ,zenash-orange))))
     `(menu                            ((t nil)))
     `(mouse                           ((t (:inherit zenash-foreground))))
     `(scroll-bar                      ((t (:background ,zenash-bg+2))))
     `(tool-bar                        ((t (:background ,zenash-bg+2))))
     `(vertical-border                 ((t (:foreground ,zenash-ll-2))))

     `(ido-first-match                 ((t (:weight bold :foreground ,zenash-selection-fg :background ,zenash-bg+2))))
     `(ido-only-match                  ((t (:weight bold :foreground ,zenash-selection-fg :background ,zenash-bg+2))))
     `(ido-subdir                      ((t (:weight bold))))

     `(isearch                         ((t (:foreground ,zenash-selection-fg :background ,zenash-blue-3))))
     `(isearch-lazy-highlight          ((t (:background ,zenash-bg-2))))

     `(idle-highlight-face             ((t (:inherit isearch-lazy-highlight))))
     
     `(hover-highlight                 ((t (:background ,zenash-bg+2))))
     `(link                            ((t (:foreground ,zenash-red-1 :weight bold))))
     `(link-visited                    ((t (:foreground ,zenash-red-1))))
     
     `(info-xref                       ((t (:foreground ,zenash-red-1 :weight bold))))
     `(info-xref-visited               ((t (:inherit info-xref :weight normal))))
     `(info-header-xref                ((t (:inherit info-xref))))
     `(info-menu-star                  ((t (:foreground ,zenash-red-1 :weight bold))))
     `(info-menu-5                     ((t (:inherit info-menu-star))))
     `(info-node                       ((t (:weight bold))))
     `(info-header-node                ((t (:weight normal))))

     `(outline-8                       ((t (:inherit default :foreground ,zenash-fg+1))))
     `(outline-7                       ((t (:inherit outline-8 :height 1.0))))
     `(outline-6                       ((t (:inherit outline-7 :height 1.0))))
     `(outline-5                       ((t (:inherit outline-6 :height 1.0))))
     `(outline-4                       ((t (:inherit outline-5 :height 1.0))))
     `(outline-3                       ((t (:inherit outline-4 :height 1.0))))
     `(outline-2                       ((t (:inherit outline-3 :height 1.0))))
     `(outline-1                       ((t (:inherit outline-2 :height 1.0))))

     `(org-hide                        ((t (:foreground ,zenash-bg))))
     `(org-special-keyword             ((t (:inherit font-lock-comment))))
     `(org-tag                         ((t (:inherit font-lock-comment))))
     `(org-table                       ((t (:background ,zenash-bg+1))))
     `(org-link                        ((t (:inherit link))))
     `(org-todo                        ((t (:inherit font-lock-variable-name))))
     `(org-formula                     ((t (:inherit font-lock-variable-name))))
     
     `(flyspell-incorrect              ((t (:inherit font-lock-warning))))
     `(flyspell-duplicate              ((t (:inherit font-lock-warning))))
     
     `(term-default-bg                 ((t (nil))))
     `(term-default-bg-inv             ((t (nil))))
     `(term-default-fg                 ((t (nil))))
     `(term-default-fg-inv             ((t (nil))))
     `(term-invisible                  ((t (nil)))) ;; FIXME: Security risk?
     `(term-invisible-inv              ((t (nil))))
     `(term-bold                       ((t (:weight bold))))
     `(term-underline                  ((t (:underline t))))

     `(eshell-prompt                   ((t (:inherit zenash-primary-1))))
     `(eshell-ls-archive               ((t (:foreground ,zenash-green :weight bold))))
     `(eshell-ls-backup                ((t (:inherit font-lock-comment))))
     `(eshell-ls-clutter               ((t (:inherit font-lock-comment))))
     `(eshell-ls-directory             ((t (:foreground ,zenash-blue :weight bold))))
     `(eshell-ls-executable            ((t (:foreground ,zenash-red :weight bold))))
     `(eshell-ls-unreadable            ((t (:inherit zenash-lowlight-1))))
     `(eshell-ls-missing               ((t (:inherit font-lock-warning))))
     `(eshell-ls-product               ((t (:inherit font-lock-doc))))
     `(eshell-ls-special               ((t (:foreground ,zenash-dark-magenta))))
     `(eshell-ls-symlink               ((t (:foreground ,zenash-cyan :weight bold))))

     
     ;; FIXME: Map these to ansi-term`s faces (`term-red`, etc.).
     `(zenash-term-dark-gray           ((t (:foreground ,zenash-dark-gray))))
     `(zenash-term-light-blue          ((t (:foreground ,zenash-light-blue))))
     `(zenash-term-light-cyan          ((t (:foreground ,zenash-light-cyan))))
     `(zenash-term-light-green         ((t (:foreground ,zenash-light-green))))
     `(zenash-term-light-magenta       ((t (:foreground ,zenash-light-magenta))))
     `(zenash-term-light-red           ((t (:foreground ,zenash-light-red))))
     `(zenash-term-light-yellow        ((t (:foreground ,zenash-light-yellow))))
     `(zenash-term-white               ((t (:foreground ,zenash-white))))

     `(zenash-term-black               ((t (:foreground ,zenash-black))))
     `(zenash-term-dark-blue           ((t (:foreground ,zenash-dark-blue))))
     `(zenash-term-dark-cyan           ((t (:foreground ,zenash-dark-cyan))))
     `(zenash-term-dark-green          ((t (:foreground ,zenash-dark-green))))
     `(zenash-term-dark-magenta        ((t (:foreground ,zenash-dark-magenta))))
     `(zenash-term-dark-red            ((t (:foreground ,zenash-dark-red))))
     `(zenash-term-dark-yellow         ((t (:foreground ,zenash-dark-yellow))))
     `(zenash-term-light-gray          ((t (:foreground ,zenash-light-gray))))

     `(plain-widget-button             ((t (:weight bold))))
     `(plain-widget-button-pressed     ((t (:inverse-video t))))
     `(plain-widget-documentation      ((t (:inherit font-lock-doc))))
     `(plain-widget-field              ((t (:background ,zenash-bg+2))))
     `(plain-widget-inactive           ((t (:strike-through t))))
     `(plain-widget-single-line-field  ((t (:background ,zenash-bg+2))))

     `(fancy-widget-button             ((t (:background ,zenash-bg+1 :box (:line-width 2 :style released-button)))))
     `(fancy-widget-button-pressed     ((t (:background ,zenash-bg+1 :box (:line-width 2 :style pressed-button)))))
     `(fancy-widget-button-highlight   ((t (:background ,zenash-bg+1 :box (:line-width 2 :style released-button)))))
     `(fancy-widget-button-pressed-highlight ((t (:background ,zenash-bg+1 :box (:line-width 2 :style pressed-button)))))
     `(fancy-widget-documentation      ((t (:inherit font-lock-doc))))
     `(fancy-widget-field              ((t (:background ,zenash-bg+2))))
     `(fancy-widget-inactive           ((t (:strike-through t))))
     `(fancy-widget-single-line-field  ((t (:background ,zenash-bg+2))))

     `(widget-button                   ((t (:inherit plain-widget-button))))
     `(widget-button-pressed           ((t (:inherit fancy-widget-button-pressed))))
     `(widget-button-highlight         ((t (:inherit fancy-widget-button-highlight))))
     `(widget-button-pressed-highlight ((t (:inherit fancy-widget-button-pressed-highlight))))
     `(widget-documentation            ((t (:inherit fancy-widget-documentation))))
     `(widget-field                    ((t (:inherit fancy-widget-field))))
     `(widget-inactive                 ((t (:inherit fancy-widget-inactive))))
     `(widget-single-line-field        ((t (:inherit fancy-widget-single-line-field))))

     `(mtorus-highlight                ((t (:inherit zenash-highlight-bluish))))
     `(mtorus-notify-highlight         ((t (:inherit zenash-primary-1))))

     `(apt-utils-normal-package        ((t (:inherit zenash-primary-1))))
     `(apt-utils-virtual-package       ((t (:inherit zenash-primary-2))))
     `(apt-utils-field-keyword         ((t (:inherit font-lock-doc))))
     `(apt-utils-field-contents        ((t (:inherit font-lock-comment))))
     `(apt-utils-summary               ((t (:inherit bold))))
     `(apt-utils-description           ((t (:inherit default))))
     `(apt-utils-version               ((t (:inherit zenash-blue))))
     `(apt-utils-broken                ((t (:inherit font-lock-warning))))

     `(breakpoint-enabled-bitmap       ((t (:inherit zenash-primary-1))))
     `(breakpoint-disabled-bitmap      ((t (:inherit font-lock-comment))))

     `(calendar-today                  ((t (:underline nil :inherit zenash-primary-2))))
     `(diary                           ((t (:underline nil :inherit zenash-primary-1))))
     `(holiday                         ((t (:underline t :inherit zenash-primary-4))))

     `(change-log-date                 ((t (:inherit zenash-blue))))

     `(comint-highlight-input          ((t (:inherit zenash-primary-1))))
     `(comint-highlight-prompt         ((t (:inherit zenash-primary-2))))

     `(compilation-info                ((t (:inherit zenash-primary-1))))
     `(compilation-warning             ((t (:inherit font-lock-warning))))

     ;; TODO
     `(cua-rectangle                   ((t (:inherit region))))

     `(custom-button                   ((t (:inherit fancy-widget-button))))
     `(custom-button-pressed           ((t (:inherit fancy-widget-button-pressed))))
     `(custom-changed                  ((t (:inherit zenash-blue))))
     `(custom-comment                  ((t (:inherit font-lock-doc))))
     `(custom-comment-tag              ((t (:inherit font-lock-doc))))
     `(custom-documentation            ((t (:inherit font-lock-doc))))
     `(custom-tag                      ((t (:inherit zenash-primary-2))))
     `(custom-group-tag                ((t (:inherit zenash-primary-1))))
     `(custom-group-tag-1              ((t (:inherit zenash-primary-4))))
     `(custom-invalid                  ((t (:inherit font-lock-warning))))
     `(custom-modified                 ((t (:inherit zenash-primary-3))))
     `(custom-rogue                    ((t (:inhrit font-lock-warning))))
     `(custom-saved                    ((t (:underline t))))
     `(custom-set                      ((t (:inverse-video t :inherit zenash-blue))))
     `(custom-state                    ((t (:inherit font-lock-comment))))
     `(custom-variable-button          ((t (:weight bold :underline t))))
     `(custom-variable-tag             ((t (:inherit zenash-primary-2))))

     `(dictionary-button               ((t (:inherit fancy-widget-button))))
     `(dictionary-reference            ((t (:inherit zenash-primary-1))))
     `(dictionary-word-entry           ((t (:inherit font-lock-keyword))))

     `(diff-header                     ((t (:inherit zenash-highlight-subtle))))
     `(diff-index                      ((t (:inherit bold))))
     `(diff-file-header                ((t (:foreground ,zenash-cyan))))
     `(diff-hunk-header                ((t (:inherit zenash-blue))))
     `(diff-added                      ((t (:inherit zenash-green))))
     `(diff-removed                    ((t (:inherit zenash-red))))
     `(diff-context                    ((t (:inherit font-lock-comment))))

     `(magit-diff-file-header           ((t (:inherit diff-file-header))))
     `(magit-diff-add                   ((t (:inherit diff-added))))
     `(magit-diff-del                   ((t (:inherit diff-removed))))
     `(magit-diff-hunk-header           ((t (:inherit diff-hunk-header))))
     `(magit-diff-none                  ((t (:inherit diff-context))))
     `(magit-item-highlight             ((t (:background ,zenash-bg-1))))
     `(magit-section-title              ((t (:inherit zenash-primary-1))))
     `(magit-branch                     ((t (:inherit font-lock-function-name :weight: bold))))
     `(magit-log-tag-label              ((t (:foreground ,zenash-blue))))
     `(magit-log-head-label             ((t (:foreground ,zenash-blue))))
     `(magit-item-mark                  ((t (:inherit isearch-lazy-highlight))))
     
     `(emms-pbi-song                   ((t (:foreground ,zenash-yellow))))
     `(emms-pbi-current                ((t (:inherit zenash-primary-1))))
     `(emms-pbi-mark-marked            ((t (:inherit zenash-primary-2))))

     `(erc-action                      ((t (:inherit erc-default))))
     `(erc-bold                        ((t (:weight bold))))
     `(erc-current-nick                ((t (:inherit zenash-primary-1))))
     `(erc-dangerous-host              ((t (:inherit font-lock-warning))))
     `(erc-default                     ((t (:foreground ,zenash-fg))))
     `(erc-direct-msg                  ((t (:inherit erc-default))))
     `(erc-error                       ((t (:inherit font-lock-warning))))
     `(erc-fool                        ((t (:inherit zenash-lowlight-1))))
     `(erc-highlight                   ((t (:inherit hover-highlight))))
     `(erc-input                       ((t (:foreground ,zenash-yellow))))
     `(erc-keyword                     ((t (:inherit zenash-primary-1))))
     `(erc-nick-default                ((t (:inherit bold))))
     `(erc-nick-msg                    ((t (:inherit erc-default))))
     `(erc-notice                      ((t (:inherit zenash-green))))
     `(erc-pal                         ((t (:inherit zenash-primary-3))))
     `(erc-prompt                      ((t (:inherit zenash-primary-2))))
     `(erc-timestamp                   ((t (:inherit zenash-green+1))))
     `(erc-underline                   ((t (:inherit underline))))

     `(ibuffer-deletion                ((t (:inherit zenash-primary-2))))
     `(ibuffer-marked                  ((t (:inherit zenash-primary-1))))
     `(ibuffer-special-buffer          ((t (:inherit font-lock-doc))))
     `(ibuffer-help-buffer             ((t (:inherit font-lock-comment))))

     `(message-cited-text              ((t (:inherit font-lock-comment))))
     ;;`(message-cited-text ((t (:foreground ,zenash-blue))))
     `(message-header-name             ((t (:inherit zenash-green+1))))
     `(message-header-other            ((t (:inherit zenash-green))))
     `(message-header-to               ((t (:inherit zenash-primary-1))))
     `(message-header-from             ((t (:inherit zenash-primary-1))))
     `(message-header-cc               ((t (:inherit zenash-primary-1))))
     `(message-header-newsgroups       ((t (:inherit zenash-primary-1))))
     `(message-header-subject          ((t (:inherit zenash-primary-2))))
     `(message-header-xheader          ((t (:inherit zenash-green))))
     `(message-mml                     ((t (:inherit zenash-primary-1))))
     `(message-separator               ((t (:inherit font-lock-comment))))

     `(gnus-header-name                ((t (:inherit message-header-name))))
     `(gnus-header-content             ((t (:inherit message-header-other))))
     `(gnus-header-from                ((t (:inherit message-header-from))))
     `(gnus-header-subject             ((t (:inherit message-header-subject))))
     `(gnus-header-newsgroups          ((t (:inherit message-header-other))))

     `(gnus-x-face                     ((t (:background ,zenash-fg :foreground ,zenash-bg))))

     ;; (gnus-cite-1 ((t (:inherit message-cited-text))))
     `(gnus-cite-1  ((t (:foreground ,zenash-blue))))
     `(gnus-cite-2  ((t (:foreground ,zenash-blue-1))))
     `(gnus-cite-3  ((t (:foreground ,zenash-blue-2))))
;;      (gnus-cite-4 ((t (:foreground ,zenash-blue-3))))
;;      (gnus-cite-5 ((t (:foreground ,zenash-blue-4))))
;;      (gnus-cite-6 ((t (:foreground ,zenash-red-4))))
;;      (gnus-cite-5 ((t (:foreground ,zenash-red-3))))
     `(gnus-cite-4  ((t (:foreground ,zenash-green+2))))
     `(gnus-cite-5  ((t (:foreground ,zenash-green+1))))
     `(gnus-cite-6  ((t (:foreground ,zenash-green))))
     `(gnus-cite-7  ((t (:foreground ,zenash-red))))
     `(gnus-cite-8  ((t (:foreground ,zenash-red-1))))
     `(gnus-cite-9  ((t (:foreground ,zenash-red-2))))
     `(gnus-cite-10 ((t (:foreground ,zenash-yellow-1))))
     `(gnus-cite-11 ((t (:foreground ,zenash-yellow))))

     `(gnus-group-mail-1           ((t (:inherit zenash-primary-1))))
     `(gnus-group-mail-2           ((t (:inherit zenash-primary-1))))
     `(gnus-group-mail-3           ((t (:inherit zenash-primary-1))))
     `(gnus-group-mail-1-empty     ((t (:inherit default))))
     `(gnus-group-mail-2-empty     ((t (:inherit default))))
     `(gnus-group-mail-3-empty     ((t (:foreground ,zenash-yellow))))
     `(gnus-group-news-1-empty     ((t (:inherit default))))
     `(gnus-group-news-2-empty     ((t (:inherit default))))
     `(gnus-group-news-3-empty     ((t (:inherit default))))

     `(gnus-signature              ((t (:foreground ,zenash-yellow))))

     `(gnus-summary-selected       ((t (:inherit zenash-primary-1))))
     `(gnus-summary-cancelled      ((t (:inherit zenash-highlight-alerting))))

     `(gnus-summary-low-ticked     ((t (:inherit zenash-primary-2))))
     `(gnus-summary-normal-ticked  ((t (:inherit zenash-primary-2))))
     `(gnus-summary-high-ticked    ((t (:inherit zenash-primary-2))))

     `(gnus-summary-low-unread     ((t (:inherit zenash-foreground :weight normal))))
     `(gnus-summary-normal-unread  ((t (:inherit zenash-foreground :weight normal))))
     `(gnus-summary-high-unread    ((t (:inherit zenash-foreground :weight bold))))

     `(gnus-summary-low-read       ((t (:inherit zenash-green :weight normal))))
     `(gnus-summary-normal-read    ((t (:inherit zenash-green :weight normal))))
     `(gnus-summary-high-read      ((t (:inherit zenash-green :weight bold))))

     `(gnus-summary-low-ancient    ((t (:inherit zenash-blue :weight normal))))
     `(gnus-summary-normal-ancient ((t (:inherit zenash-blue :weight normal))))
     `(gnus-summary-high-ancient   ((t (:inherit zenash-blue))))

     `(help-argument-name          ((t (:weight bold))))

     ;; See also the variable definitions at the top of this file
     `(imaxima-latex-error         ((t (:inherit font-lock-warning))))

     `(jabber-roster-user-chatty   ((t (:inherit zenash-primary-1))))
     `(jabber-roster-user-online   ((t (:inherit zenash-primary-2))))
     `(jabber-roster-user-away     ((t (:inherit font-lock-doc))))
     `(jabber-roster-user-xa       ((t (:inherit font-lock-comment))))
     `(jabber-roster-user-offline  ((t (:inherit zenash-lowlight-1))))
     `(jabber-roster-user-dnd      ((t (:inherit zenash-primary-5))))
     `(jabber-roster-user-error    ((t (:inherit font-lock-warning))))

     `(jabber-title-small          ((t (:inherit zenash-title :height 1.2))))
     `(jabber-title-medium         ((t (:inherit jabber-title-small :height 1.2))))
     `(jabber-title-large          ((t (:inherit jabber-title-medium :height 1.2))))

     `(jabber-chat-prompt-local    ((t (:inherit zenash-primary-1))))
     `(jabber-chat-prompt-foreign  ((t (:inherit zenash-primary-2))))

     `(jde-java-font-lock-modifier ((t (:inherit zenash-primary-2))))
     `(jde-java-font-lock-doc-tag  ((t (:inherit zenash-primary-1))))
     `(jde-java-font-lock-constant ((t (:inherit font-lock-constant))))
     `(jde-java-font-lock-package  ((t (:inherit zenash-primary-3))))
     `(jde-java-font-lock-number   ((t (:inherit font-lock-constant))))
     `(jde-java-font-lock-operator ((t (:inherit font-lock-keyword))))
     `(jde-java-font-lock-link     ((t (:inherit zenash-primary-5 :underline t))))

     `(keywiz-right                ((t (:inherit zenash-primary-1))))
     `(keywiz-wrong                ((t (:inherit font-lock-warning))))
     `(keywiz-command              ((t (:inherit zenash-primary-2))))

     `(font-latex-bold             ((t (:inherit bold))))
     `(font-latex-warning          ((t (:inherit font-lock-warning))))
     `(font-latex-sedate           ((t (:inherit zenash-primary-1))))
     `(font-latex-title-4          ((t (:inherit zenash-title))))

     `(makefile-space              ((t (:inherit font-lock-warning))))
     `(makefile-shell              ((t (nil))))
     ;; This does not work very well because everything that`s highlighted
     ;; inside the shell region will get its own box.
     ;; (makefile-shell ((t (:background "#4f4f4f"
     ;;                           :box (:line-width 2 :color "#4f4f4f")))))

     `(nxml-delimited-data                   ((t (:inherit font-lock-string))))
     `(nxml-name                             ((t (:inherit zenash-primary-1))))
     `(nxml-ref                              ((t (:inherit zenash-primary-5))))
     `(nxml-delimiter                        ((t (:inherit default))))
     `(nxml-text                             ((t (:inherit default))))

     `(nxml-comment-content                  ((t (:inherit font-lock-comment))))
     `(nxml-comment-delimiter                ((t (:inherit nxml-comment-content))))
     `(nxml-processing-instruction-target    ((t (:inherit zenash-primary-2))))
     `(nxml-processing-instruction-delimiter ((t (:inherit nxml-processing-instruction-target))))
     `(nxml-processing-instruction-content   ((t (:inherit nxml-processing-instruction-target))))
     `(nxml-cdata-section-CDATA              ((t (:inherit zenash-primary-4))))
     `(nxml-cdata-section-delimiter          ((t (:inherit nxml-cdata-section-CDATA))))
     `(nxml-cdata-section-content            ((t (:inherit nxml-text))))
     `(nxml-entity-ref-name                  ((t (:inherit zenash-primary-5))))
     `(nxml-entity-ref-delimiter             ((t (:inherit nxml-entity-ref-name))))
     `(nxml-char-ref-number                  ((t (:inherit nxml-entity-ref-name))))
     `(nxml-char-ref-delimiter               ((t (:inherit nxml-entity-ref-delimiter))))

     `(nxml-tag-delimiter                    ((t (:inherit default))))
     `(nxml-tag-slash                        ((t (:inherit default))))
     `(nxml-element-local-name               ((t (:inherit zenash-primary-1))))
     `(nxml-element-prefix                   ((t (:inherit default))))
     `(nxml-element-colon                    ((t (:inherit default))))

     `(nxml-attribute-local-name             ((t (:inherit zenash-primary-3))))
     `(nxml-namespace-attribute-prefix       ((t (:inherit nxml-attribute-local-name))))
     `(nxml-attribute-value                  ((t (:inherit font-lock-string))))
     `(nxml-attribute-value-delimiter        ((t (:inherit nxml-attribute-value))))
     `(nxml-attribute-prefix                 ((t (:inherit default))))
     `(nxml-namespace-attribute-xmlns        ((t (:inherit nxml-attribute-prefix))))
     `(nxml-attribute-colon                  ((t (:inherit default))))
     `(nxml-namespace-attribute-colon        ((t (:inherit nxml-attribute-colon))))

     ;; TODO
     `(setnu-line-number                     ((t (:inherit zenash-lowlight-2))))

     `(speedbar-button                       ((t (:inherit zenash-primary-1))))
     `(speedbar-file                         ((t (:inherit zenash-primary-2))))
     `(speedbar-directory                    ((t (:inherit zenash-primary-5))))
     `(speedbar-tag                          ((t (:inherit font-lock-function-name))))
     `(speedbar-highlight                    ((t (:underline t))))

     `(strokes-char                          ((t (:inherit font-lock-keyword))))

     `(todoo-item-header                     ((t (:inherit zenash-primary-1))))
     `(todoo-item-assigned-header            ((t (:inherit zenash-primary-2))))
     `(todoo-sub-item-header                 ((t (:foreground ,zenash-yellow))))

     `(tuareg-font-lock-governing            ((t (:inherit zenash-primary-2))))
     `(tuareg-font-lock-interactive-error    ((t (:inherit font-lock-warning))))
     `(tuareg-font-lock-interactive-output   ((t (:inherit zenash-primary-3))))
     `(tuareg-font-lock-operator             ((t (:inherit font-lock-operator))))

     `(w3m-form-button                       ((t (:inherit widget-button))))
     `(w3m-form-button-pressed               ((t (:inherit widget-button-pressed))))
     `(w3m-form-button-mouse                 ((t (:inherit widget-button-pressed))))
     `(w3m-tab-unselected                    ((t (:box (:line-width 1 :style released-button)))))
     `(w3m-tab-selected                      ((t (:box (:line-width 1 :style pressed-button)))))
     `(w3m-tab-unselected-retrieving         ((t (:inherit (w3m-tab-unselected widget-inactive)))))
     `(w3m-tab-selected-retrieving           ((t (:inherit (w3m-tab-selected widget-inactive)))))
     `(w3m-tab-background                    ((t (:inherit zenash-highlight-subtle))))
     `(w3m-anchor                            ((t (:inherit zenash-primary-1))))
     `(w3m-arrived-anchor                    ((t (:inherit zenash-primary-2))))
     `(w3m-image                             ((t (:inherit zenash-primary-4))))
     `(w3m-form                              ((t (:inherit widget-field)))))

     (zenash-make-face-alias-clauses
      '(Buffer-menu-buffer-face
        apt-utils-broken-face
        apt-utils-description-face
        apt-utils-field-contents-face
        apt-utils-field-keyword-face
        apt-utils-normal-package-face
        apt-utils-summary-face
        apt-utils-version-face
        apt-utils-virtual-package-face
        breakpoint-disabled-bitmap-face
        breakpoint-enabled-bitmap-face
        calendar-today-face
        change-log-date-face
        compilation-info-face
        compilation-warning-face
        cua-rectangle-face
        custom-button-face
        custom-button-pressed-face
        custom-changed-face
        custom-comment-face
        custom-comment-tag-face
        custom-documentation-face
        custom-face-tag-face
        custom-group-tag-face
        custom-group-tag-face-1
        custom-invalid-face
        custom-modified-face
        custom-rogue-face
        custom-saved-face
        custom-set-face
        custom-state-face
        custom-variable-button-face
        custom-variable-tag-face
        diary-face
        dictionary-button-face
        dictionary-reference-face
        dictionary-word-entry-face
        diff-added-face
        diff-context-face
        diff-file-header-face
        diff-header-face
        diff-hunk-header-face
        diff-index-face
        diff-removed-face
        emms-pbi-current-face
        emms-pbi-mark-marked-face
        emms-pbi-song-face
        erc-action-face
        erc-bold-face
        erc-current-nick-face
        erc-dangerous-host-face
        erc-default-face
        erc-direct-msg-face
        erc-error-face
        erc-fool-face
        erc-highlight-face
        erc-input-face
        erc-keyword-face
        erc-nick-default-face
        erc-nick-msg-face
        erc-notice-face
        erc-pal-face
        erc-prompt-face
        erc-timestamp-face
        erc-underline-face
        eshell-ls-archive-face
        eshell-ls-backup-face
        eshell-ls-clutter-face
        eshell-ls-directory-face
        eshell-ls-executable-face
        eshell-ls-missing-face
        eshell-ls-product-face
        eshell-ls-special-face
        eshell-ls-symlink-face
        eshell-ls-unreadable-face
        eshell-prompt-face
        fancy-widget-button-face
        fancy-widget-button-highlight-face
        fancy-widget-button-pressed-face
        fancy-widget-button-pressed-highlight-face
        fancy-widget-documentation-face
        fancy-widget-field-face
        fancy-widget-inactive-face
        fancy-widget-single-line-field-face
        font-latex-bold-face
        font-latex-sedate-face
        font-latex-title-4-face
        font-latex-warning-face
        font-lock-builtin-face
        font-lock-comment-delimiter-face
        font-lock-comment-face
        font-lock-constant-face
        font-lock-doc-face
        font-lock-function-name-face
        font-lock-keyword-face
        font-lock-negation-char-face
        font-lock-operator-face
        font-lock-preprocessor-face
        font-lock-pseudo-keyword-face
        font-lock-string-face
        font-lock-type-face
        font-lock-variable-name-face
        font-lock-warning-face
        gnus-cite-face-1
        gnus-cite-face-10
        gnus-cite-face-11
        gnus-cite-face-2
        gnus-cite-face-3
        gnus-cite-face-4
        gnus-cite-face-5
        gnus-cite-face-6
        gnus-cite-face-7
        gnus-cite-face-8
        gnus-cite-face-9
        gnus-group-mail-1-empty-face
        gnus-group-mail-2-empty-face
        gnus-group-mail-3-empty-face
        gnus-group-mail-3-face
        gnus-group-news-1-empty-face
        gnus-group-news-2-empty-face
        gnus-group-news-3-empty-face
        gnus-header-content-face
        gnus-header-from-face
        gnus-header-name-face
        gnus-header-newsgroups-face
        gnus-header-subject-face
        gnus-signature-face
        gnus-summary-cancelled-face
        gnus-summary-high-ancient-face
        gnus-summary-high-read-face
        gnus-summary-high-ticked-face
        gnus-summary-high-unread-face
        gnus-summary-low-ancient-face
        gnus-summary-low-read-face
        gnus-summary-low-ticked-face
        gnus-summary-low-unread-face
        gnus-summary-normal-ancient-face
        gnus-summary-normal-read-face
        gnus-summary-normal-ticked-face
        gnus-summary-normal-unread-face
        gnus-summary-selected-face
        highlight-current-line-face
        holiday-face
        ibuffer-deletion-face
        ibuffer-help-buffer-face
        ibuffer-marked-face
        ibuffer-special-buffer-face
        ido-first-match-face
        ido-only-match-face
        ido-subdir-face
        imaxima-latex-error-face
        isearch-lazy-highlight-face
        jde-java-font-lock-constant-face
        jde-java-font-lock-doc-tag-face
        jde-java-font-lock-link-face
        jde-java-font-lock-modifier-face
        jde-java-font-lock-number-face
        jde-java-font-lock-operator-face
        jde-java-font-lock-package-face
        keywiz-command-face
        keywiz-right-face
        keywiz-wrong-face
        makefile-shell-face
        makefile-space-face
        message-cited-text-face
        message-header-cc-face
        message-header-from-face
        message-header-name-face
        message-header-newsgroups-face
        message-header-other-face
        message-header-subject-face
        message-header-to-face
        message-header-xheader-face
        message-mml-face
        message-separator-face
        mtorus-highlight-face
        mtorus-notify-highlight-face
        nxml-attribute-colon-face
        nxml-attribute-local-name-face
        nxml-attribute-prefix-face
        nxml-attribute-value-delimiter-face
        nxml-attribute-value-face
        nxml-cdata-section-CDATA-face
        nxml-cdata-section-content-face
        nxml-cdata-section-delimiter-face
        nxml-char-ref-delimiter-face
        nxml-char-ref-number-face
        nxml-comment-content-face
        nxml-comment-delimiter-face
        nxml-delimited-data-face
        nxml-delimiter-face
        nxml-element-colon-face
        nxml-element-local-name-face
        nxml-element-prefix-face
        nxml-entity-ref-delimiter-face
        nxml-entity-ref-name-face
        nxml-name-face
        nxml-namespace-attribute-colon-face
        nxml-namespace-attribute-prefix-face
        nxml-namespace-attribute-xmlns-face
        nxml-processing-instruction-content-face
        nxml-processing-instruction-delimiter-face
        nxml-processing-instruction-target-face
        nxml-ref-face
        nxml-tag-delimiter-face
        nxml-tag-slash-face
        nxml-text-face
        paren-face
        plain-widget-button-face
        plain-widget-button-pressed-face
        plain-widget-documentation-face
        plain-widget-field-face
        plain-widget-inactive-face
        plain-widget-single-line-field-face
        setnu-line-number-face
        show-paren-match-face
        show-paren-mismatch-face
        speedbar-button-face
        speedbar-directory-face
        speedbar-file-face
        speedbar-highlight-face
        speedbar-tag-face
        strokes-char-face
        todoo-item-assigned-header-face
        todoo-item-header-face
        todoo-sub-item-header-face
        tuareg-font-lock-governing-face
        tuareg-font-lock-interactive-error-face
        tuareg-font-lock-interactive-output-face
        tuareg-font-lock-operator-face
        w3m-anchor-face
        w3m-arrived-anchor-face
        w3m-form-button-face
        w3m-form-button-mouse-face
        w3m-form-button-pressed-face
        w3m-form-face
        w3m-image-face
        w3m-tab-background-face
        w3m-tab-selected-face
        w3m-tab-selected-retrieving-face
        w3m-tab-unselected-face
        w3m-tab-unselected-retrieving-face
        widget-button-face
        widget-button-highlight-face
        widget-button-pressed-face
        widget-button-pressed-highlight-face
        widget-documentation-face
        widget-field-face
        widget-inactive-face
        widget-single-line-field-face))
    )))

;;;###autoload
(defalias 'zenash #'color-theme-zenash)

(provide 'zenash)
;;; zenash.el ends here.
