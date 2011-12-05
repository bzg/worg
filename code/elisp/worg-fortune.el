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
;; Example: Quotes (max 120 characters) in a custom format:
;;
;;(worg-write-fortune-file "~/install/git/worg/org-quotes.org"
;;                          "/srv/http/org-mode/fortunes" 120
;;                          "quotes=\"%s\"" "\n")
;; 
;;; Code:

(defvar worg-fortune nil)

(defun worg-write-fortune-file (src dest limit &optional fmt sep)
  "Collect fortunes from SRC file and write them to DEST file.
LIMIT is the maximum size of a fortune to be added.  Optional
fourth argument FMT is a format to apply to the inserted quote, 
and optional fifth argument SEP is the separator to use."
  (find-file src)
  (worg-collect-fortune-from-buffer)
  (find-file dest)
  (erase-buffer)
  (let (f)
    (while (setq f (pop worg-fortune))
      (when (< (length f) limit)
	(insert (format (or fmt "%s") f))
	(insert (or sep "\n%\n")))))
  (write-file dest))

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
	fortune (replace-regexp-in-string "\\\\" "" fortune)
	;; fortune (replace-regexp-in-string "\n" " " fortune)
	)
  (with-temp-buffer
    (insert fortune)
    (goto-char (point-min))
    (while (re-search-forward org-bracket-link-analytic-regexp nil t)
      (replace-match (match-string 5)))
    (goto-char (point-max))
    (beginning-of-line)
    (when (looking-at "^ +")
      (replace-match ""))
    (insert "  -- ")
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match ""))
    (setq fortune (buffer-string))))

(provide 'worg-fortune)


