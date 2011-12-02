;;; org-issue.el --- Simple mailing list based issue tracker for Org mode
;;
;; Author: David Maus <dmaus [at] ictsoc.de>
;;
;; Copyright (C) 2010 by David Maus
;;
;; This file is NOT part of Gnu Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; History:
;;
;; 2010-11-07  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-link-gmane): Create link to mid
;;   resolver, not find_root.
;; 
;; 2010-08-21  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-url-escape): New function.
;;   (org-issue-get-msginfo:gnus, org-issue-get-msginfo:wl): Use
;;   function.
;; 
;; 2010-08-08  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-template-body): Fix capture template
;;   body.
;; 
;; 2010-08-07  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-new): Insert newline after new capture
;;   entry.
;; 
;; 2010-08-04  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-new): Immediate finish capture
;;   template.
;; 
;; 2010-08-02  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-new): Use org-capture instead of
;;   org-remember.
;; 
;; 2010-07-25  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-update-message-flag): Keep flag for NEW
;;   issues only.
;; 
;; 2010-07-23  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-template-body): Don't indent PROPERTIES
;;   drawer.
;; 
;; 2010-07-21  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-template-body): Add blank line after
;;   Gmane link.
;; 
;; 2010-07-02  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-bulk-update-message-flag): New function.
;; 
;; 2010-06-27  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-display): Fix typo.
;;   (org-issue-remove-ml-prefix): Set return value.
;; 
;; 2010-06-24  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-display): Move point in other window.
;;   (org-issue-remove-ml-prefix): New function.
;;   (org-issue-get-msginfo:gnus, org-issue-get-msginfo:wl): Remove
;;   Org mode mailing list prefix.
;; 
;; 2010-06-22  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-change-todo): New function.  Change
;;   TODO keyword of issue.
;;   (org-issue-display): New function.  Display issue in other
;;   window.
;;   (org-issue-jump): New function.  Jump to issue.
;; 
;; 2010-06-15  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el (org-issue-tag): Save buffer before kill.
;;   (org-issue-close): Proper call to `org-issue-flag-message'.
;;   (org-issue-update-message-flag): Only remove message flag if
;;   issue is not in TODO state.
;;   (org-issue-update-message-flag): Proper call to
;;   `org-issue-flag-message'.
;; 
;; 2010-06-13  David Maus  <dmaus@ictsoc.de>
;; 
;;   * org-issue.el: Initial revision.
;; 
;;; Commentary:
;;
;; This file contains helper functions to maintain Org mode's issue
;; file from within Wanderlust and Gnus.
;;
;; Available functions:
;;
;; `org-issue-new': File a new issue for current message.  Create a new
;;                  TODO in `org-issue-issue-file' below the headline
;;                  "New Issues" with keyword NEW.  If customization
;;                  variable `org-issue-message-flag' is non-nil and
;;                  flagging messages is supported, the current issue
;;                  is flagged.
;;
;; `org-issue-close': Close issue of current message.
;;
;; `org-issue-tag'  : Tag issue of current message.
;;
;; `org-issue-update-message-flag' : Update message flag according to
;;                                   issue file.  If the issue for
;;                                   current message is closed or
;;                                   turned into a development task,
;;                                   the message flag is removed.
;;
;; `org-issue-link-gmane' : An Org mode web link pointing to current
;;                          message on gmane is pushed to kill-ring and
;;                          clipboard.
;;

;;; Code:
(defcustom org-issue-issue-file "~/code/org-mode/worg/org-issues.org"
  "Path to Org mode's issue file."
  :type 'file
  :group 'org-issue)

(defcustom org-issue-message-flag 'issue
  "Flag that indicates an issue.
Set this to nil if you do not want messages to be flagged.  The
flag is added or removed by the functions `org-issue-new',
`org-issue-close',  and `org-issue-update'."
  :type 'symbol
  :group 'org-issue)

(defun org-issue-replace-brackets (s)
  "Return S with all square brackets replaced by parentheses."
  (while (string-match "\\[" s)
    (setq s (replace-match "(" nil nil s)))
  (while (string-match "\\]" s)
    (setq s (replace-match ")" nil nil s)))
  s)

(defun org-issue-remove-ml-prefix (s)
  "Return S without Org mode mailing list prefix."
  (if (string-match "^\\[Orgmode\\] " s)
      (setq s (replace-match "" nil nil s)))
  s)

(defun org-issue-get-msginfo ()
  "Return a cons with message id in car and subject in cdr."
  (cond
   ((eq major-mode 'wl-summary-mode)
    (org-issue-get-msginfo:wl))
   ((memq major-mode '(gnus-summary-mode gnus-article-mode))
    (org-issue-get-msginfo:gnus))
   (t
    (error "Unsupported mailer mode: %s" major-mode))))

(defun org-issue-url-escape (s)
  "Escape chars in S for gmane's id resolver."
  (mapconcat (lambda (chr)
	       (if (or (and (> chr 64) (< chr 91))
		       (and (> chr 96) (< chr 123))
		       (and (> chr 47) (< chr 58)))
		   (char-to-string chr)
		 (format "%%%X" chr))) s ""))

(defun org-issue-get-msginfo:gnus ()
  "Return a cons with message id in car and subject in cdr.
Operates on Gnus messages."
  (let ((header (with-current-buffer gnus-summary-buffer
		  (gnus-summary-article-header))))
    (cons
     (org-issue-url-escape
      (org-remove-angle-brackets
       (mail-header-id header)))
     (org-issue-replace-brackets
      (org-issue-remove-ml-prefix
       (mail-header-subject header))))))

(defun org-issue-get-msginfo:wl ()
  "Return a cons with message id in car and subject in cdr.
Operates on Wanderlust messages."
  (let* ((num (wl-summary-message-number))
	 (ent (if (fboundp 'elmo-message-entity)
		  (elmo-message-entity
		   wl-summary-buffer-elmo-folder num)
		(elmo-msgdb-overview-get-entity
		 num (wl-summary-buffer-msgdb)))))
    (cons (org-issue-url-escape
	   (org-remove-angle-brackets
	    (org-wl-message-field 'message-id ent)))
	  (org-issue-replace-brackets
	   (org-issue-remove-ml-prefix
	    (org-wl-message-field 'subject ent))))))

(defun org-issue-exists-p (id)
  "Return non-nil, if an issue identified by ID exists."
  (let ((visiting (find-buffer-visiting org-issue-issue-file))
	e)
    (with-current-buffer (or visiting
			     (find-file-noselect org-issue-issue-file))
      (setq e (org-find-entry-with-id (format "mid:%s" id)))
      (unless visiting (kill-buffer)))
    e))

(defun org-issue-link-gmane (&optional msginfo)
  "Return web link to gmane for current message.
If called interactively, the link is also pushed to clipboard and
kill-ring."
  (interactive)
  (let* ((msginfo (or msginfo (org-issue-get-msginfo)))
	 (gmane (format
		 "[[http://mid.gmane.org/%s][%s]]"
		 (car msginfo) (cdr msginfo))))
    (if (called-interactively-p)
	(org-kill-new gmane)
      (when (fboundp 'x-set-selection)
	(ignore-errors (x-set-selection 'PRIMARY gmane))
	(ignore-errors (x-set-selection 'CLIPBOARD gmane))))
    gmane))

(defun org-issue-template-body (msginfo)
  "Return string with remember template body.
MSGINFO is a cons with message id in car and message subject in
cdr."
  (concat
   "* NEW " (cdr msginfo) "\n"
   "  %u\n"
   ":PROPERTIES:\n"
   ":ID: mid:" (car msginfo) "\n"
   ":END:\n\n"
   "    - Gmane :: " (org-issue-link-gmane msginfo) "\n\n"))

(defun org-issue-new ()
  "File new issue for current message."
  (interactive)
  (let* ((msginfo (org-issue-get-msginfo))
	 (org-capture-templates
	  `(("i" "Issue"
	     entry (file+headline ,org-issue-issue-file "New issues")
	     ,(org-issue-template-body msginfo)
	     :immediate-finish t :empty-lines 1))))
    (if (org-issue-exists-p (car msginfo))
	(error "Already filed: %s" (cdr msginfo))
      (if org-issue-message-flag
	  (org-issue-flag-message org-issue-message-flag))
      (org-capture))))

(defun org-issue-flag-message (flag &optional remove)
  "Flag current message.
FLAG is the desired message flag.
If optional argument REMOVE is non-nil, remove the flag."
  (cond
   ((eq major-mode 'wl-summary-mode)
    (org-issue-flag-message:wl flag remove))
   (t
    (error "Unsupported mailer mode: %s" major-mode))))

(defun org-issue-flag-message:wl (flag remove)
  "Flag current Wanderlust message."
  (let* ((num (wl-summary-message-number))
	 (folder wl-summary-buffer-elmo-folder)
	 (flags (elmo-get-global-flags
		 (elmo-message-flags folder num))))
    (elmo-message-set-global-flags
     folder num (if remove (delq flag flags)
		  (if (memq flag flags) flags (cons flag flags))))))

(defun org-issue-tag ()
  "Tag issue of current message."
  (interactive)
  (let ((msginfo (org-issue-get-msginfo))
	(visiting (find-buffer-visiting org-issue-issue-file)))
    (unless (org-issue-exists-p (car msginfo))
      (error "No such issue: %s" (cdr msginfo)))
    (with-current-buffer (or visiting
			     (find-file-noselect org-issue-issue-file))
      (save-excursion
	(goto-char (org-find-entry-with-id (format "mid:%s" (car msginfo))))
	(org-set-tags-command))
      (save-buffer)
      (unless visiting (kill-buffer)))))

(defun org-issue-keyword ()
  "Change TODO keyword of current message."
  (interactive)
  (let ((msginfo (org-issue-get-msginfo))
	(visiting (find-buffer-visiting org-issue-issue-file)))
    (unless (org-issue-exists-p (car msginfo))
      (error "No such issue: %s" (cdr msginfo)))
    (with-current-buffer (or visiting
			     (find-file-noselect org-issue-issue-file))
      (goto-char (org-find-entry-with-id (format "mid:%s" (car msginfo))))
      (call-interactively 'org-todo))))

(defun org-issue-display ()
  "Display issue in other-window."
  (interactive)
  (let ((msginfo (org-issue-get-msginfo))
	(buf (or (find-buffer-visiting org-issue-issue-file)
		 (find-file-noselect org-issue-issue-file)))
	wn pt)
    (unless (org-issue-exists-p (car msginfo))
      (error "No such issue: %s" (cdr msginfo)))
    (setq wn (display-buffer buf 'other-window))
    (with-current-buffer buf
      (setq pt (org-find-entry-with-id (format "mid:%s" (car msginfo))))
      (goto-char pt)
      (org-reveal))
    (set-window-point wn pt)))

(defun org-issue-jump ()
  "Jump to issue of current message."
  (interactive)
  (let ((msginfo (org-issue-get-msginfo))
	(buf (or (find-buffer-visiting org-issue-issue-file)
		 (find-file-noselect org-issue-issue-file))))
    (switch-to-buffer buf)
    (goto-char (org-find-entry-with-id (format "mid:%s" (car msginfo))))
    (org-reveal)))

(defun org-issue-close ()
  "Close issue of current message."
  (interactive)
  (let ((msginfo (org-issue-get-msginfo))
	(visiting (find-buffer-visiting org-issue-issue-file)))
    (unless (org-issue-exists-p (car msginfo))
      (error "No such issue: %s" (cdr msginfo)))
    (with-current-buffer (or visiting
			     (find-file-noselect org-issue-issue-file))
      (save-excursion
	(goto-char (org-find-entry-with-id (format "mid:%s" (car msginfo))))
	(org-todo 'done))
      (unless visiting (kill-buffer)))
    (if org-issue-message-flag
	(org-issue-flag-message org-issue-message-flag t))))

(defun org-issue-update-message-flag ()
  "Update message flag according to issue file."
  (interactive)
  (let ((msginfo (org-issue-get-msginfo))
	(visiting (find-buffer-visiting org-issue-issue-file))
	state)
    (unless (org-issue-exists-p (car msginfo))
      (error "No such issue: %s" (cdr msginfo)))
    (with-current-buffer (or visiting
			     (find-file-noselect org-issue-issue-file))
      (save-excursion
	(goto-char (org-find-entry-with-id (format "mid:%s" (car msginfo))))
	(setq state (org-get-todo-state)))
      (unless visiting (kill-buffer)))
    (org-issue-flag-message
     org-issue-message-flag
     (or (null state) (not (string= state "NEW"))))))

(defun org-issue-bulk-update-message-flag ()
  "Update message flag of all messages in summary."
  (interactive)
  (when (eq major-mode 'wl-summary-mode)
    (goto-char (point-min))
    (while (not (eobp))
      (ignore-errors (org-issue-update-message-flag))
      (beginning-of-line 2))))

(provide 'org-issue)

;;; org-issue.el ends here
