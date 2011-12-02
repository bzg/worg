;;; worg.el --- this is code for Worg
;;
;; Copyright 2008 Bastien Guerry
;;
;; Emacs Lisp Archive Entry
;; Filename: worg.el
;; Version: 0.1
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
;;
;;; Commentary:
;;
;; This file provides dynamic blocks for editing Worg pages.
;; See the Worg project here: http://orgmode.org/w/worg.git
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'worg)
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(defun org-dblock-write:timestamp (params)
  "Insert a simple timestamp.
Params are: string (like \"Updated: \"
            format (the formatting string)"
  (let ((string (or (plist-get params :string) "Updated: "))
	(time-format (or (plist-get params :format)
			 (car org-time-stamp-formats))))
    (insert string (format-time-string time-format))))

(provide 'worg)

;;;  User Options, Variables



;;; worg.el ends here
