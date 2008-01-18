;;; org-toc.el --- Table of contents for Org-mode buffer
;;
;; Copyright 2007 2008 Bastien Guerry
;;
;; Emacs Lisp Archive Entry
;; Filename: org-toc.el
;; Version: 0.9d
;; Author: Bastien Guerry <bzg AT altern DOT org>
;; Maintainer: Bastien Guerry <bzg AT altern DOT org>
;; Keywords: org, wp, toc
;; Description: Shows a browsable table of contents for Org buffer
;; URL: http://www.cognition.ens.fr/~guerry/u/org-toc.el
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This library implements a browsable table of contents for Org files.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-toc)

;;; Code:

(eval-when-compile
  (require 'cl))

;;; Custom variables:

(defface org-toc-background
  '((((background light)) (:background "#EEEEEE"))
    (((background dark)) (:background "#111111")))
  "Face used as the background for Org TOC buffer."
  :group 'org-faces)

(defgroup org-toc nil
  "Options concerning the browsable table of contents of Org-mode."
  :tag "Org TOC"
  :group 'org)

(defcustom org-toc-default-depth 1
  "Default depth when invoking `org-toc-show' without argument."
  :group 'org-toc
  :type '(choice
	  (const :tag "same as base buffer" nil)
	  (integer :tag "level")))

(defcustom org-toc-follow-mode nil
  "Non-nil means navigating through the table of contents will
move the point in the Org buffer accordingly."
  :group 'org-toc
  :type 'boolean)

(defcustom org-toc-info-mode nil
  "Non-nil means navigating through the table of contents will
show the properties for the current headline in the echo-area."
  :group 'org-toc
  :type 'boolean)

(defcustom org-toc-show-subtree-mode nil
  "Non-nil means show subtree when going to headline or following
it while browsing the table of contents."
  :group 'org-toc
  :type '(choice
	  (const :tag "show subtree" t)
	  (const :tag "show entry" nil)))

(defcustom org-toc-recenter-mode t
  "Non-nil means recenter the Org buffer when following the
headlines in the TOC buffer."
  :group 'org-toc
  :type 'boolean)

(defcustom org-toc-recenter 0
  "Where to recenter the Org buffer when unfolding a subtree.
This variable is only used when `org-toc-recenter-mode' is set to
'custom. A value >=1000 will call recenter with no arg."
  :group 'org-toc
  :type 'integer)

(defcustom org-toc-info-exclude '("ALLTAGS")
  "A list of excluded properties when displaying info in the
echo-area. The COLUMNS property is always exluded."
  :group 'org-toc
  :type 'list)

;; TODO: add the number of line for this heading?
(defcustom org-toc-margin-default-format "%5l"
  "The default margin format. This is overriden by any occurrence
of #+TOC_MARGIN: in the base Org buffer.

%l stands for the heading's line number.
%h stands for the number of headings below this heading.
%t stands for the number of TODO items below this heading.

For example, a format like \"%5l hl:%2h td:%2t | \" would show:

 1456 hl: 2 td: 1 | * A headline here

- this headline is on line 1456
- there are 2 headlines below it
- there are 1 TODO item among these 2 headlines

Attention: using a complex margin format for big Org files could
slow down things considerabily."
  :group 'org-toc
  :type 'string)

;;; Variables

(defvar org-toc-base-buffer nil)
(defvar org-toc-columns-shown nil)
(defvar org-toc-odd-levels-only nil)
(defvar org-toc-todo-regexp nil)
(defvar org-toc-margin-current-format nil)
(defvar org-toc-config-alist nil)
(defvar org-toc-cycle-global-status nil)

(defvar org-toc-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map [remap self-insert-command] 'windresize-other-char)
    (suppress-keymap map)
    ;; toggle modes
    (define-key map "f" 'org-toc-follow-mode)
    (define-key map "S" 'org-toc-show-subtree-mode)
    (define-key map "s" 'org-toc-store-config)
    (define-key map "g" 'org-toc-restore-config)
    (define-key map "i" 'org-toc-info-mode)
    (define-key map "r" 'org-toc-recenter-mode)
    ;; navigation keys
    (define-key map "p" 'org-toc-previous)
    (define-key map "n" 'org-toc-next)
    (define-key map [(left)] 'org-toc-previous)
    (define-key map [(right)] 'org-toc-next)
    (define-key map [(up)] 'org-toc-previous)
    (define-key map [(down)] 'org-toc-next)
    (define-key map "1" (lambda() (interactive) (org-toc-show 1 (point))))
    (define-key map "2" (lambda() (interactive) (org-toc-show 2 (point))))
    (define-key map "3" (lambda() (interactive) (org-toc-show 3 (point))))
    (define-key map "4" (lambda() (interactive) (org-toc-show 4 (point))))
    (define-key map " " 'org-toc-goto)
    (define-key map "q" 'org-toc-quit)
    ;; (define-key map "x" 'org-toc-quit)
    ;; go to the location and stay in the base buffer
    (define-key map [(tab)] 'org-toc-jump)
    ;; (define-key map "v" 'org-toc-jump)
    ;; go to the location and delete other windows
    (define-key map [(return)]
      (lambda() (interactive) (org-toc-jump t)))
    ;; special keys
    (define-key map "c" 'org-toc-columns)
    (define-key map "?" 'org-toc-help)
    (define-key map ":" 'org-toc-cycle-subtree)
    (define-key map "\C-c\C-o" 'org-open-at-point)
    ;; global cycling in the base buffer
    (define-key map (kbd "C-S-<iso-lefttab>")
      'org-toc-cycle-base-buffer)
    ;; subtree cycling in the base buffer
    (define-key map [(control tab)]
      (lambda() (interactive) (org-toc-goto nil t)))
    map)
  "Keymap for `org-toc-mode'.")

(defalias 'org-show-table-of-contents 'org-toc-show)

;;; Org TOC mode:

(defun org-toc-mode ()
  "A major mode for browsing the table of contents of an Org buffer.

\\{org-toc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map org-toc-mode-map)
  (setq mode-name "Org TOC")
  (setq major-mode 'org-toc-mode)
  (easy-menu-add org-toc-menu)
  (org-toc-refresh-overlays))

(defun org-toc-refresh-overlays ()
  "Refresh overlays in the Org TOC buffer."
  (save-excursion
    (goto-char (point-min))
    (let* ((margin org-toc-margin-current-format)
	   (line (string-match "%-?\\(?:[0-9]*\\)l" margin))
	   (sub-h (string-match "%-?\\(?:[0-9]*\\)h" margin))
	   (sub-t (string-match "%-?\\(?:[0-9]*\\)t" margin))
	   margin-string)
      (while (re-search-forward (concat "^" outline-regexp ".*$") nil t)
	(let ((ov (org-make-overlay (match-beginning 0) (match-end 0)))
	      table)
	  (and margin
	       (setq 
		table
		(delete 
		 nil
		 (list
		  (if line (cons "%l" (line-number-at-pos
				       (match-beginning 0))))
		  (if sub-h (cons "%h" (org-toc-count-sub-headings)))
		  (if sub-t (cons "%t" (org-toc-count-sub-headings t))))))
	       (setq margin-string (org-replace-escapes margin table))
	       (set-text-properties 0 (length margin-string) nil margin-string)
	       (org-overlay-put ov 'before-string margin-string))
	  ;; FIXME the margin disappear if hidestars is on
	  (org-overlay-put ov 'face 'org-toc-background))))))

(easy-menu-define org-toc-menu org-toc-mode-map "Org TOC"
  '("Org TOC"
    ("Navigate"
     ["Next heading" org-toc-next t]
     ["Previous heading" org-toc-previous t]
     ["Follow heading" org-toc-goto t]
     ["Go to heading" org-toc-jump t]
     ["Jump to heading" (org-toc-jump t) :keys "RET"]
     ["Quit the TOC" org-toc-quit t])
    "--"
    ["Follow headings" org-toc-follow-mode 
     :style toggle :selected org-toc-follow-mode]
    ["Display info" org-toc-info-mode 
     :style toggle :selected org-toc-info-mode]
    ["Show full subtree" org-toc-show-subtree-mode 
     :style toggle :selected org-toc-show-subtree-mode]
    ["Recenter" org-toc-recenter-mode 
     :style toggle :selected org-toc-recenter-mode]
    ["Columns view" org-toc-columns 
     :style toggle :selected org-toc-columns-shown]
    "--"
    ["Store configuration" org-toc-store-config t]
    ["Restore configuration" org-toc-restore-config t]
    "--"
    ["Cycle subtree in TOC" org-toc-cycle-subtree t]
    ["Global cycle in Org" org-toc-cycle-base-buffer 
     :keys "C-S-TAB"]
    ["Cycle subtree in Org" (lambda() (interactive) (org-toc-goto nil t)) 
     :keys "C-TAB"]))

;;; Toggle functions:

(defun org-toc-follow-mode ()
  "Toggle follow mode in a `org-toc-mode' buffer."
  (interactive)
  (setq org-toc-follow-mode (not org-toc-follow-mode))
  (message "Follow mode is %s"
	   (if org-toc-follow-mode "on" "off")))

(defun org-toc-info-mode ()
  "Toggle info mode in a `org-toc-mode' buffer."
  (interactive)
  (setq org-toc-info-mode (not org-toc-info-mode))
  (message "Info mode is %s"
	   (if org-toc-info-mode "on" "off")))

(defun org-toc-show-subtree-mode ()
  "Toggle show subtree mode in a `org-toc-mode' buffer."
  (interactive)
  (setq org-toc-show-subtree-mode (not org-toc-show-subtree-mode))
  (message "Show subtree mode is %s"
	   (if org-toc-show-subtree-mode "on" "off")))

(defun org-toc-recenter-mode (&optional line)
  "Toggle recenter mode in a `org-toc-mode' buffer. If LINE is
specified, then make `org-toc-recenter' use this value."
  (interactive "P")
  (setq org-toc-recenter-mode (not org-toc-recenter-mode))
  (when (numberp line)
    (setq org-toc-recenter-mode t)
    (setq org-toc-recenter line))
  (message "Recenter mode is %s"
	   (if org-toc-recenter-mode
	       (format "on, line %d" org-toc-recenter) "off")))

(defun org-toc-cycle-subtree ()
  "Locally cycle a headline through two states: 'children and
'folded"
  (interactive)
  (let ((beg (point))
	(end (save-excursion (end-of-line) (point)))
	(ov (car (org-overlays-at (point))))
	status)
    (if ov (setq status (org-overlay-get ov 'status))
      (setq ov (org-make-overlay beg end)))
    ;; change the folding status of this headline
    (cond ((or (null status) (eq status 'folded))
	   (show-children)
	   (message "CHILDREN")
	   (org-overlay-put ov 'status 'children))
	  ((eq status 'children)
	   (show-branches)
	   (message "BRANCHES")
	   (org-overlay-put ov 'status 'branches))
	  (t (hide-subtree)
	     (message "FOLDED")
	     (org-overlay-put ov 'status 'folded)))))

;;; Main show function:
;; FIXME name this org-before-first-heading-p?
(defun org-toc-before-first-heading-p ()
  "Before first heading?"
  (save-excursion
    (null (re-search-backward (concat "^" outline-regexp) nil t))))

(defvar org-toc-previous-window-configuration nil)

;;;###autoload
(defun org-toc-show (&optional depth position)
  "Show the table of contents of the current Org-mode buffer."
  (interactive "P")  
  (setq org-toc-previous-window-configuration 
	(current-window-configuration))
  (if (org-mode-p)
      (progn (setq org-toc-base-buffer (current-buffer))
	     (setq org-toc-odd-levels-only org-odd-levels-only)
	     (setq org-toc-todo-regexp org-todo-regexp)
	     (setq org-toc-margin-current-format
		   (or (save-excursion
			 (goto-char (point-min))
			 (if (re-search-forward 
			      "^#\\+TOC_MARGIN: *\\(.*\\)$" nil t)
			     (match-string 1) nil)) 
		       org-toc-margin-default-format)))
    (if (eq major-mode 'org-toc-mode)
	(switch-to-buffer org-toc-base-buffer)
      (error "Not in an Org buffer")))
  ;; create the new window display
  (let ((pos (or position
		 (save-excursion
		   (if (org-toc-before-first-heading-p)
		       (progn (re-search-forward (concat "^" outline-regexp) nil t)
			      (match-beginning 0))
		     (point))))))
    (setq org-toc-cycle-global-status org-cycle-global-status)
    (delete-other-windows)
    (and (get-buffer "*org-toc*") (kill-buffer "*org-toc*"))
    (switch-to-buffer-other-window
     (make-indirect-buffer org-toc-base-buffer "*org-toc*"))
  ;; set outline-regexp to "\\*+ "
    (org-set-local 'outline-regexp org-outline-regexp)
    ;; make content before 1st headline invisible
    (goto-char (point-min))
    (let* ((beg (point-min))
	   (end (and (re-search-forward (concat "^" outline-regexp) nil t)
		     (1- (match-beginning 0))))
	   (ov (org-make-overlay beg end))
	   (help (format "Table of contents for %s (press ? for a quick help):\n"
			 (buffer-name org-toc-base-buffer))))
      (org-overlay-put ov 'invisible t)
      (org-overlay-put ov 'before-string help))
    ;; build the browsable TOC
    (cond (depth
	   (let* ((dpth (if org-toc-odd-levels-only
			    (1- (* depth 2)) depth)))
	     (org-content dpth)
	     (setq org-toc-cycle-global-status
		   `(org-content ,dpth))))
	   ((null org-toc-default-depth)
	    (if (eq org-toc-cycle-global-status 'overview)
		(progn (org-overview)
		       (setq org-cycle-global-status 'overview)
		       (run-hook-with-args 'org-cycle-hook 'overview))
	      (progn (org-overview)
		     (org-content)
		     (setq org-cycle-global-status 'contents)
		     (run-hook-with-args 'org-cycle-hook 'contents))))
	   (t (let* ((dpth0 org-toc-default-depth)
		     (dpth (if org-toc-odd-levels-only
			       (1- (* dpth0 2)) dpth0)))
		(org-content dpth)
		(setq org-toc-cycle-global-status
		      `(org-content ,dpth)))))
    (goto-char pos))
  (move-beginning-of-line nil)
  (org-toc-mode)
  (shrink-window-if-larger-than-buffer)
  (setq buffer-read-only t))

(defun org-toc-count-sub-headings (&optional todo match)
  "Count the number of headings below the heading at point.
If TODO, restrict to TODO headlines.
If MATCH, restrict to headlines matching MATCH."
  (save-excursion
    (beginning-of-line)
    (let ((level (progn (looking-at "^\\(\*+\\) ")
			(length (match-string 1))))
	  (end (save-excursion (outline-end-of-subtree) (point)))
	  (cnt 0))
      (if org-toc-odd-levels-only
	  (setq level (number-to-string (1+ (/ (1+ level) 2))))
	(setq level (number-to-string (1+ level))))
      (while (re-search-forward 
	      (concat "^\\*\\{" level "\\} " 
		      (if todo org-toc-todo-regexp)
		      "\\(.*\\)$") end t)
	(unless (and match (not (string-match match (match-string 1))))
	  (setq cnt (+ 1 cnt))))
      cnt)))

;;; Navigation functions:
(defun org-toc-goto (&optional jump cycle)
  "From Org TOC buffer, follow the targeted subtree in the Org window.
If JUMP is non-nil, go to the base buffer.  
If JUMP is 'delete, go to the base buffer and delete other windows.
If CYCLE is non-nil, cycle the targeted subtree in the Org window."
  (interactive)
  (let ((pos (point))
	(toc-buf (current-buffer)))
    (switch-to-buffer-other-window org-toc-base-buffer)
    (goto-char pos)
    (if cycle (org-cycle)
      (progn (org-overview)
	     (if org-toc-show-subtree-mode
		 (org-show-subtree)
	       (org-show-entry))
	     (org-show-context)))
    (if org-toc-recenter-mode
	(if (>= org-toc-recenter 1000) (recenter)
	  (recenter org-toc-recenter)))
    (cond ((null jump)
	   (switch-to-buffer-other-window toc-buf))
	  ((eq jump 'delete)
	   (delete-other-windows)))))

(defun org-toc-cycle-base-buffer ()
  "Call `org-cycle' with a prefix argument in the base buffer."
  (interactive)
  (switch-to-buffer-other-window org-toc-base-buffer)
  (org-cycle t)
  (other-window 1))

(defun org-toc-jump (&optional delete)
  "From Org TOC buffer, jump to the targeted subtree in the Org window.
If DELETE is non-nil, delete other windows when in the Org buffer."
  (interactive "P")
  (if delete (org-toc-goto 'delete)
    (org-toc-goto t)))

(defun org-toc-previous ()
  "Go to the previous headline of the TOC."
  (interactive)
  (if (save-excursion
	  (beginning-of-line)
	  (re-search-backward (concat "^" outline-regexp) nil t))
    (outline-previous-visible-heading 1)
    (message "No previous heading"))
  (if org-toc-info-mode (org-toc-info))
  (if org-toc-follow-mode (org-toc-goto)))

(defun org-toc-next ()
  "Go to the next headline of the TOC."
  (interactive)
  (if (save-excursion 
	(outline-next-visible-heading 1)
	(looking-at (concat "^" outline-regexp)))
      (outline-next-visible-heading 1)
    (message "No next heading"))
  (if org-toc-info-mode (org-toc-info))
  (if org-toc-follow-mode (org-toc-goto)))

(defun org-toc-quit ()
  "Quit the current Org TOC buffer."
  (interactive)
  (set-window-configuration 
   org-toc-previous-window-configuration))
;; ;;   (kill-this-buffer)
;; ;;   (other-window 1)
;; ;;   (delete-other-windows)
;;   )

;;; Special functions:
(defun org-toc-columns ()
  "Toggle columns view in the Org buffer from Org TOC."
  (interactive)
  (let ((indirect-buffer (current-buffer)))
    (switch-to-buffer org-toc-base-buffer)
    (if (not org-toc-columns-shown)
	(progn (org-columns)
	       (setq org-toc-columns-shown t))
      (progn (org-columns-remove-overlays)
	     (setq org-toc-columns-shown nil)))
    (switch-to-buffer indirect-buffer)))

(defun org-toc-info ()
  "Show properties of current subtree in the echo-area."
  (interactive)
  (let ((pos (point))
	(indirect-buffer (current-buffer))
	props prop msg)
    (switch-to-buffer org-toc-base-buffer)
    (goto-char pos)
    (setq props (org-entry-properties))
    (while (setq prop (pop props))
      (unless (or (equal (car prop) "COLUMNS")
		  (member (car prop) org-toc-info-exclude))
	(let ((p (car prop))
	      (v (cdr prop)))
	  (if (equal p "TAGS")
	      (setq v (mapconcat 'identity (split-string v ":" t) " ")))
	  (setq p (concat p ":"))
	  (add-text-properties 0 (length p) '(face org-special-keyword) p)
	  (setq msg (concat msg p " " v "  ")))))
    (switch-to-buffer indirect-buffer)
    (message msg)))

;;; Store and restore TOC configuration:
(defun org-toc-store-config ()
  "Store the current status of the tables of contents in
`org-toc-config-alist'."
  (interactive)
  (let ((file (buffer-file-name org-toc-base-buffer))
	(pos (point))
	(hlcfg (org-toc-get-headlines-status)))
    (setq org-toc-config-alist
	  (delete (assoc file org-toc-config-alist)
		  org-toc-config-alist))
    (add-to-list 'org-toc-config-alist
		 `(,file ,pos ,org-toc-cycle-global-status ,hlcfg))
    (message "TOC configuration saved: (%s)"
	     (if (listp org-toc-cycle-global-status)
		 (concat "org-content "
			 (number-to-string
			  (cadr org-toc-cycle-global-status)))
	       (symbol-name org-toc-cycle-global-status)))))

(defun org-toc-restore-config ()
  "Get the stored status in `org-toc-config-alist' and set the
current table of contents to it."
  (interactive)
  (let* ((file (buffer-file-name org-toc-base-buffer))
	 (conf (cdr (assoc file org-toc-config-alist)))
	 (pos (car conf))
	 (status (cadr conf))
	 (hlcfg (caddr conf)) hlcfg0 ov)
    (cond ((listp status)
	   (org-toc-show (cadr status) (point)))
	  ((eq status 'overview)
	   (org-overview)
	   (setq org-cycle-global-status 'overview)
	   (run-hook-with-args 'org-cycle-hook 'overview))
	  (t
	   (org-overview)
	   (org-content)
	   (setq org-cycle-global-status 'contents)
	   (run-hook-with-args 'org-cycle-hook 'contents)))
    (while (setq hlcfg0 (pop hlcfg))
      (save-excursion
	(goto-char (point-min))
	(when (search-forward (car hlcfg0) nil t)
	  (setq ov (org-make-overlay (match-beginning 0)
				     (match-end 0)))
	  (cond ((eq (cdr hlcfg0) 'children)
		 (show-children)
		 (message "CHILDREN")
		 (org-overlay-put ov 'status 'children))
		((eq (cdr hlcfg0) 'branches)
		 (show-branches)
		 (message "BRANCHES")
		 (org-overlay-put ov 'status 'branches))))))
    (goto-char pos)
    (if org-toc-follow-mode (org-toc-goto))
    (message "Last TOC configuration restored")
    (sit-for 1)
    (if org-toc-info-mode (org-toc-info))))

(defun org-toc-get-headlines-status ()
  "Return an alist of headlines and their associated folding
status."
  (let (output ovs)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
		  (goto-char (next-overlay-change (point))))
	(when (looking-at (concat "^" outline-regexp))
	  (add-to-list
	   'output
	   (cons (buffer-substring-no-properties
		  (match-beginning 0)
		  (save-excursion
		    (end-of-line) (point)))
		 (overlay-get
		  (car (overlays-at (point))) 'status))))))
    ;; return an alist like (("* Headline" . 'status))
    output))

;; In Org TOC buffer, hide headlines below the first level.
(defun org-toc-help ()
  "Display a quick help message in the echo-area for `org-toc-mode'."
  (interactive)
  (let ((st-start 0) 
	(help-message
	 "\[space]   follow heading                   [1-4] hide headlines below this level
\[TAB]     go to heading                    [f]   toggle follow mode (currently %s)
\[return]  jump to heading                  [i]   toggle info mode (currently %s)
\[C-TAB]   cycle subtree (in Org)           [S]   toggle show subtree mode (currently %s)
\[C-S-TAB] global cycle (in Org)            [r]   toggle recenter mode (currently %s)   
\[:]       cycle subtree (in TOC)           [c]   toggle column view (currently %s)
\[n/p]     next/previous heading            [s]   save TOC configuration 
\[q]       quit the TOC                     [g]   restore last TOC configuration"))
    (while (string-match "\\[[^]]+\\]" help-message st-start)
      (add-text-properties (match-beginning 0)
                           (match-end 0) '(face bold) help-message)
      (setq st-start (match-end 0)))
  (message help-message
    (if org-toc-follow-mode "on" "off")
    (if org-toc-info-mode "on" "off")
    (if org-toc-show-subtree-mode "on" "off")
    (if org-toc-recenter-mode (format "on, line %s" org-toc-recenter) "off")
    (if org-toc-columns-shown "on" "off"))))

(provide 'org-toc)

;;;  User Options, Variables

;;; org-toc.el ends here
