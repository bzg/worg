;;; org-collector --- collect properties into tables
;;
;; Copyright (C) 2008 Eric Schulte
;;
;; Emacs Lisp Archive Entry
;; Filename: org-collector.el
;; Version: 0.1
;; Author: Eric Schulte <schulte.eric AT gmail DOT com>
;; Keywords: org, properties, collection, tables
;; Description: collect properties into tables
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
;;; Comments:
;;
;; Pass in an alist of columns, each column can be either a single
;; property or a function which takes properties as arguments.  A
;; table will be populated by passing proerty values to each of the
;; column specifiers.  There will be one row in the table for each
;; headline which satisfies your colum specifiers.  An example dblock
;; specification with results may look like this.
;;
;; #+BEGIN: propview :id "data" :cols (ITEM f d list (apply '* list) (+ f d))
;; | "ITEM" | "f" | "d" | "list"                  | "(apply (quote *) list)" | "(+ f d)" |
;; |--------+-----+-----+-------------------------+--------------------------+-----------|
;; | "run1" |   2 |  33 | (quote (9 2 3 4 5 6 7)) | 45360                    |        35 |
;; | "run2" |   4 |  34 | :na                     | :na                      |        38 |
;; | "run3" |   4 |  35 | :na                     | :na                      |        39 |
;; | "run4" |   2 |  36 | :na                     | :na                      |        38 |
;; |        |     |     |                         |                          |           |
;; #+END:
;; 
;;; History:
;;
;; Simplified org-propview-to-table and made unquoted headers (removed
;; extra format %S call). /mfo 2008-12-16
;;
;; Added a :no-inherit feature to gain speed together with some
;; documentation. /mfo 2008-11-25
;;
;;; Code:
(require 'org)
(require 'org-table)

(defun and-rest (list)
  (if (listp list)
      (if (> (length list) 1)
	  (and (car list) (and-rest (cdr list)))
	(car list))
    list))

(put 'org-collector-error
     'error-conditions
     '(error column-prop-error org-collector-error))

(defun org-read-prop (prop)
  "Convert the string property PROP to a number if appropriate.
Otherwise if prop looks like a list (meaning it starts with a
'(') then read it as lisp, otherwise return it unmodified as a
string."
  (if (and (stringp prop) (not (equal prop "")))
      (let ((out (string-to-number prop)))
	(if (equal out 0)
	    (if (or (equal "(" (substring prop 0 1)) (equal "'" (substring prop 0 1)))
		(read prop)
	      (if (string-match "^\\(+0\\|-0\\|0\\)$" prop)
		  0
		(progn (set-text-properties 0 (length prop) nil prop)
		       prop)))
	  out))
    prop))

(defun org-dblock-write:propview (params)
  "Generates org-collector propview table.

It collects the column specifications from the :cols parameter
preceeding the dblock, then update the contents of the dblock
with data from headings selected by the :id parameter. It can be:

 * global      - data from whole document is processed
 * local       - only current subtree
 * <org-id>    - only headings with this property :ID:.

If no inheritance is wanted set paramter :no-inherit, to gain
speed."
  (interactive)
  (condition-case er
      (let* ((cols (plist-get params :cols))
	     (id (plist-get params :id))
	     (inherit (not (plist-get params :no-inherit)))
	     (org-use-tag-inheritance inherit)
	     (org-use-property-inheritance inherit)
	     table idpos)
	(save-excursion
	  (cond ((not id) nil)
		((eq id 'global)
		 (goto-char (point-min))
		 (outline-next-heading))
		((eq id 'local)  nil)
		((setq idpos (org-find-entry-with-id id))
		 (goto-char idpos))
		(t (error "Cannot find entry with :ID: %s" id)))
	  (org-narrow-to-subtree)
	  (setq table (org-propview-to-table (org-propview-collect cols)))
	  (widen))
	(insert table)
	(org-cycle))
    (org-collector-error (widen) (error "%s" er))
    (error (widen) (error "%s" er))))

(defun org-propview-collect (cols)
  (interactive)
  ;; collect the properties from every header
  (let* ((header-props (org-map-entries (quote (cons (cons "ITEM" (org-get-heading))
						     (org-entry-properties)))))
	 ;; collect all property names
	 (prop-names (mapcar 'intern (delete-dups
				      (apply 'append (mapcar (lambda (header)
							       (mapcar 'car header))
							     header-props))))))
    ;; (message (format "header-props=%S" header-props))
    ;; (message (format "prop-names=%S" prop-names))
    (append
     (list
      ;; create an output list of the headers for each output col
      cols
      'hline)
     (mapcar ;; for each header's entries
      (lambda (props)
	(mapcar ;;   for each col
	 (lambda (col)
	   (or
	    ;; if col is a symbol and it's present return it's value
	    (and (symbolp col)
		 (let ((val (cdr (assoc (symbol-name col) props))))
		   (if val (org-read-prop val))))
	    ;; if col is a list, and everything in it's cdr is present,
	    ;; then evaluate it as a function
	    (and (listp col)
		 (let ((vals (mapcar (lambda (el) (if (memq el prop-names)
						      (org-read-prop (cdr (assoc (symbol-name el) props)))
						      el))
				     (cdr col))))
		   ;; (message (format "vals-%S" vals))
		   (condition-case col-er
		       (and (and-rest vals) (org-read-prop (eval (cons (car col) vals))))
		     (error (signal 'org-collector-error
				    (list (format "%S while processing: %S" col-er col)))))))
	    :na)) ;; else return an appropriate default
	 cols))
      header-props))))

(defun org-propview-to-table (results)
  (orgtbl-to-orgtbl results '(:fmt "%S" :remove-nil-lines)))

(provide 'org-collector)
;;; org-collector ends here
