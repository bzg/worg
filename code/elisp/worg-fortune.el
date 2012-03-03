;;; worg-fortune.el --- export Worg quotes into fortune file
;;
;; Copyright (C) 2011 Bastien Guerry, Inc.
;;
;; Author: Bastien Guerry <bzg AT gnu DOT org>
;; Maintainer: Bastien Guerry <bzg AT gnu DOT org>
;; Keywords: org, worg, quote, fortune
;; Description: export Worg quotes into fortune file

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Example: Fortunes limited to 120 characters:
;;
;; (worg-write-fortune-file "~/install/git/worg/org-quotes.org"
;;                          "/srv/http/org-mode/fortunes" 120)
;; 
;; This is the function that is used to create the javascript
;; code on http://orgmode.org that inserts a random quote:
;; 
;; (worg-write-fortune-file 
;;  "~/install/git/worg/org-quotes.org" 
;;  "/srv/http/org-mode/org-quote.js"
;;  130
;;  "r_text[%d] = \"%s\";" "\n"
;;  'worg-fortune-insert-javascript-pre
;;  'worg-fortune-insert-javascript-post)
;; 
;;; Code:

;; List where to store the fortune strings
(defvar worg-fortune nil)

;; Counter that can also be used in preamble or postamble
(defvar worg-fortune-cnt 0)

(defun worg-write-fortune-file (src dest limit &optional fmt sep pre post)
  "Collect fortunes from SRC file and write them to DEST file.
LIMIT is the maximum size of a fortune to be added.

Optional fourth argument FMT is a format to apply to the inserted
quote, and optional fifth argument SEP is the separator to use.
For now, the format should contain both %d and %s format strings,
in this order.

PRE and POST are a preambule and a postamble to the fortune file.
They can be either a string or a function which will be applied
in the DEST buffer."
  (save-window-excursion
    (find-file src)
    (setq worg-fortune nil worg-fortune-cnt 0)
    (worg-collect-fortune-from-buffer)
    (find-file dest)
    (erase-buffer)
    ;; Insert preamble
    (cond ((functionp pre) (funcall pre))
	  ((stringp pre) (insert pre)))
    ;; insert fortune strings
    (let (f)
      (while (setq f (pop worg-fortune))
	(when (< (length f) limit)
	  (insert (if fmt (format fmt worg-fortune-cnt f) f))
	  (insert (or sep "\n%\n"))
	  (setq worg-fortune-cnt (1+ worg-fortune-cnt)))))
    ;; Insert postamble
    (cond ((functionp post) (funcall post))
	  ((stringp post) (insert post)))
    (write-file dest)))

(defun worg-collect-fortune-from-buffer nil
  "Collect a buffer's fortunes into `worg-fortune'."
  (interactive)
  ;; Make sure we are in org-mode
  (org-mode)
  (setq worg-fortune nil)
  (goto-char (point-min))
  (while (re-search-forward "^#\\+begin_quote.*$" nil t)
    (let* ((start (1+ (match-end 0)))
	   (end (progn (re-search-forward "^#\\+end_quote.*$" nil t)
		       (1- (match-beginning 0))))
	   (f (buffer-substring-no-properties start end)))
      (setq f (worg-fortune-cleanup f))
      (add-to-list 'worg-fortune f t))))

(defun worg-fortune-cleanup (fortune)
  "Clean up HTML and Org elements in FORTUNE."
  (setq fortune (replace-regexp-in-string "@<[^>]+>" "" fortune)
	fortune (replace-regexp-in-string "\\\\" "" fortune))
  (with-temp-buffer
    (insert fortune)
    (goto-char (point-min))
    (while (re-search-forward org-bracket-link-analytic-regexp nil t)
      (replace-match (match-string 5)))
    (goto-char (point-max))
    (beginning-of-line)
    (when (looking-at "^ +")
      (replace-match ""))
    (insert " -- ")
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match " "))
    (setq fortune (buffer-string))))

(defun worg-fortune-insert-javascript-pre ()
  (goto-char (point-min))
  (insert "var r_text = new Array ();\n"))

(defun worg-fortune-insert-javascript-post ()
  (goto-char (point-max))
  (insert (format "var i = Math.floor(%d*Math.random())\n" 
		  worg-fortune-cnt)
	  "document.write(r_text[i]);"))

(provide 'worg-fortune)

