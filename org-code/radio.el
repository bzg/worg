;;; radio.el --- annotate any file with arbitrary org-mode subtrees

;; Copyright (C) 2007  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'org)

;; NOTE: The external program `uuidgen' from e2fsprogs is required.
;; You probably also need GNU Emacs 22.

;; <:541efa2c-35e1-491a-a57f-3fc67708d43b:> 

;; This program lets you associate an arbitrary org-mode subtree with
;; any line of any text file (or with a binary file.) The subtrees are
;; kept in a separate org file. It provides an interactive command
;; `radio-edit-annotation' (bound to C-c , e) that will bring up the
;; annotation. You can enter tasks, log time, and do all the other
;; things you love about org-mode, and you won't clutter up your
;; original document with notes and todo items, or mess around with
;; multiple major modes.

;; Press C-M-c when you are done editing, and the notes will be saved
;; before you return to what you were doing. By default, the notes are
;; saved in file.org for a given file.extension, but this can be
;; changed on a local or global basis. 

;; When a line is annotated, it has a colored "<:" at the end, and
;; pressing C-c , e jumps back to the existing annotation.

;; To use it:

;;  1. open a text file of source code or something
;;  2. M-x radio-mode RET
;;  3. choose a line (empty or not) and hit C-c , e
;;  4. Type/edit your org annotations
;;  5. C-M-c to finish and save
;;  6. Try entering todo's and then C-c , t 

;; This isn't very complete yet---I want to make the tags interactive
;; so that they can display more information about the data in the
;; radio buffer, and possibly make creative uses of the header line.

;; I hope I can use this start making more effective use of my coding
;; time; I often wander around a file or project looking for things to
;; do. Org-mode is designed for collecting task data from many files
;; and organizing your work time, but org-mode naturally only works in
;; org-mode. So radio-mode lets you attach org-mode data that can be
;; searched later. Because it's a full org-mode buffer, you can have
;; various kinds of todo keywords (think of BUG, KNOWNCAUSE, FIXED,
;; WONTFIX)

(defun radio-make-uuid ()
  "Generate a UUID string by invoking the external program `uuidgen'.
Debian users will find it in the e2fsprogs package."
  (with-temp-buffer
    (when (= 0 (call-process "uuidgen" nil t))
      (buffer-substring-no-properties (point-min)
				      ;; remove trailing newline
				      (1- (point-max))))))

(defvar radio-headline-regexp "^* \\(\\([a-f0-9]+-\\)+[a-f0-9]+\\)")

(make-variable-buffer-local
 (defvar radio-custom-file nil
   "When this variable is non-nil in a buffer, then this file
name is used as the annotation file instead of the default
foo.radio.org. This can be used to keep annotations from many
files in one file."))

(put 'radio-custom-file 'safe-local-variable #'stringp)

(defun* radio-file (&optional (file (buffer-file-name))) 
  (or radio-custom-file (concat file ".radio")))

(make-variable-buffer-local
 (defvar *radio-buffer* nil "The associated org-mode buffer."))

(make-variable-buffer-local
 (defvar radio-buffer-p nil "When non-nil in a buffer, this is a radio buffer."))

(defun radio-buffer () 
  (if (buffer-live-p *radio-buffer*)
      *radio-buffer*
      (setf *radio-buffer* (save-window-excursion 
				 (let ((buffer (find-file (radio-file))))
				   (prog1 buffer 
				     (setf radio-buffer-p t)))))))

(defconst radio-annotation-tag-regexp (concat "\\(<" ":\\)\\(.*\\)\\(:" ">\\)")
  "Regular expression matching an
notations.")

(defun radio-format-annotation-tag (string)
  "Format STRING as an annotation."
  (concat "<" ":" string ":" ">"))

(defun radio-read-next-annotation-tag (&optional bound)
  (when (re-search-forward radio-annotation-tag-regexp bound :noerror)
    (match-string-no-properties 2)))

(defun radio-find-annotation-headline (uuid)
  (re-search-forward (concat "^\* " uuid) nil :noerror))

(make-variable-buffer-local
 (defvar *radio-annotation-cache* (make-hash-table :test 'equal)
   "A hash table mapping UUID strings to org properties."))

(defun radio-clear-cache ()
  (setf *radio-annotation-cache* (make-hash-table :test 'equal)))

;; (radio-clear-cache)

(defun radio-annotation-properties (uuid)
  (gethash uuid *radio-annotation-cache*))

(defun radio-rescan-annotation-properties ()
  (interactive)
  (let (plists uuid)
    (with-current-buffer (radio-buffer)
      (save-excursion
	(save-restriction 
	  (save-match-data 
	    (widen)
	    (goto-char (point-min))
	    (while (re-search-forward radio-headline-regexp nil :noerror)
	      (setf uuid (match-string-no-properties 1))
	      (let* ((beg (point-at-bol))
		     (end (or (and (re-search-forward radio-headline-regexp nil :noerror)
				   (point-at-bol))
			      (point-max))))
		(goto-char beg)
		;; collect all subtree entries
		(while (re-search-forward outline-regexp end :noerror)
		  (push (org-entry-properties) plists))))))))
    (puthash uuid plists *radio-annotation-cache*)))

(defun radio-debug-print ()
  (interactive)
  (with-current-buffer "FOO"
    (delete-region (point-min) (point-max))
    (maphash (lambda (k v)
	       (insert (format "%S" (list k v)) "\n"))
	     *radio-annotation-cache*)))

;;; Editing annotations

(defun radio-do-recursive-edit ()
  (setf header-line-format "Annotating. Press C-M-c (exit-recursive-edit) when finished.")
  (recursive-edit)
  (setf header-line-format nil))

(defun radio-edit-annotation ()
  "Edit the annotation on the current line."
  (interactive)  
  (when radio-buffer-p 
    (error "Cannot annotate a radio buffer."))
  (save-window-excursion 
    (beginning-of-line)
    ;; see if there's already an annotation link for this line.
    (let ((uuid (radio-read-next-annotation-tag (point-at-eol))))
      (when (null uuid)
	;; no. insert a new annotation.
	(setf uuid (radio-make-uuid))
	(end-of-line)		
	(comment-dwim nil)			
	(insert (radio-format-annotation-tag uuid)))
      ;; we may need to insert the annotation heading in the org buffer.
      (switch-to-buffer-other-window (radio-buffer)) 
      (org-mode)
      (goto-char (point-min))
      (if (radio-find-annotation-headline uuid)
	  ;; nope, just move down and get ready to receive input
	  (newline)
	  ;; insert the annotation heading
	  (progn 
	    (goto-char (point-min))	
	    (insert "* " uuid "\n\n")
	    (beginning-of-line) 
	    (previous-line)))		
      (org-narrow-to-subtree)
      (org-show-subtree)	
      (org-show-context)
      (radio-do-recursive-edit)
      (with-current-buffer (radio-buffer) 
	(save-buffer))
      (widen)))
  (radio-rescan-annotation-properties))

(defun radio-edit-first-annotation ()
  "Edit the first annotation in the file; this can be used to
hold notes that relate to the file as a whole."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward radio-annotation-tag-regexp nil :noerror)
      (beginning-of-line)
      (radio-edit-annotation))))

;;; Annotating binaries

;; So far you can only annotate the file as a whole. I imagine it
;; wouldn't be hard to annotate individual positions or ranges in an
;; audio file, by storing time indexes in the radio file (instead of
;; UUIDs) and writing special code to make ecasound play those parts.

(defun radio-edit-binary-annotation (file)
  (interactive "fAnnotate Binary File: ")
  (save-window-excursion
    (find-file-other-window (radio-file file))
    (org-mode)
    (radio-do-recursive-edit)
    (save-buffer)))

;;; Getting a list of all TODO's 

;; This is very basic but it works.

(defun radio-show-todo-list ()
  (interactive)
  (let ((org-agenda-files (list (radio-file))))
      (org-todo-list 1)))
		 
;;; Displaying useful information instead of the UUID's

(defvar radio-use-overlays t "When non-nil, use overlays to hide the UUID's
The overlaid text is controlled by `radio-overlay-format'..")

(defun radio-format-overlay (uuid)
  (propertize "" 'face '(:foreground "beige")))
  
(defvar radio-format-overlay-function #'radio-format-overlay)

;;; Font-locking stuff

(defface radio-annotation-delimiter-face
'((t (:foreground "gold3")))
  "Face for radio tags.")

(defvar radio-annotation-delimiter-face
'radio-annotation-delimiter-face)

(defface radio-annotation-delimiter-alt-face
    '((t (:foreground "gray40")))
  "Face for radio tags.")

(defvar radio-annotation-delimiter-alt-face
  'radio-annotation-delimiter-alt-face)

(defface radio-annotation-data-face
'((t (:foreground "gray35")))
  "Face for radio tag data.")

(defvar radio-annotation-data-face
  'radio-annotation-data-face)

(defface radio-attention-face
'((t (:foreground "red")))
  "Face for things that should get your attention.")

(defvar radio-attention-face
  'radio-attention-face)

(defvar radio-font-lock-keywords 
  `((,radio-annotation-tag-regexp 
     (1 radio-annotation-delimiter-face prepend)
     (2 (prog1 radio-annotation-data-face
	  (when radio-use-overlays 
	    (overlay-put (make-overlay (match-end 1)
				       (match-end 3))
			 'display (funcall radio-format-overlay-function 
					   (match-string 2)))))
	prepend)
     (3 radio-annotation-delimiter-alt-face prepend))))

(defun radio-do-font-lock (add-or-remove)
  (dolist (keyword radio-font-lock-keywords)
    (apply add-or-remove (list nil (list keyword)))))

(defun radio-enable ()
  (radio-do-font-lock 'font-lock-add-keywords)
  (font-lock-fontify-buffer))

(defun radio-disable ()
  (radio-do-font-lock 'font-lock-remove-keywords)
  (font-lock-fontify-buffer))

;;; Minor mode for hiding the ugly UUID's, plus a quick keybinding

(defvar radio-keymap nil)
(when (null radio-keymap)
  (setq radio-keymap (make-sparse-keymap))
  (define-key radio-keymap (kbd "C-c , e") 'radio-edit-annotation)
  (define-key radio-keymap (kbd "C-c , f") 'radio-edit-first-annotation)
  (define-key radio-keymap (kbd "C-c , t") 'radio-show-todo-list)
  (define-key radio-keymap (kbd "C-c , b") 'radio-edit-binary-annotation))

(define-minor-mode radio-mode
  "Tag lines of a file with Org entries kept in another file."
  nil 				
  :lighter " Radio"
  :keymap radio-keymap
  (if radio-mode
      (radio-enable)
    (radio-disable)))

;; (add-hook 'emacs-lisp-mode-hook #'radio-enable)

(provide 'radio)
;;; radio.el ends here
