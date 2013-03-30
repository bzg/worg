;;; org-license.el --- Add a license to your org files

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Author: David Arroyo Menéndez <davidam@es.gnu.org>
;; Keywords: license, creative commons
;; Homepage: http://orgmode.org
;;
;; This file is not part of GNU Emacs, yet.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements functions to add a license fast in org files.
;; Org-mode doesn't load this module by default - if this is not what
;; you want, configure the variable `org-modules'. Thanks to #emacs-es
;; irc channel for your support.

;;; Code:


(defun org-license-cc-by (language)
  (interactive "MLanguage: " language)
  (cond ((equal language "es")
	 (insert "* Licencia
Esta página está bajo una [[http://creativecommons.org/licenses/by/3.0/deed][Licencia Creative Commons Reconocimiento Unported 3.0]]"))
	(t (insert "* License
This page is under a [[http://creativecommons.org/licenses/by/3.0/deed][Creative Commons Attribution Unported 3.0]]")))
  (insert "\n[[file:http://i.creativecommons.org/l/by/3.0/88x31.png]]"))

(defun org-license-cc-by-sa (language)
  (interactive "MLanguage: " language)
  (cond ((equal language "es") 
	 (insert "* Licencia
Esta página está bajo una [[http://creativecommons.org/licenses/by-sa/3.0/deed][Licencia Creative Commons Reconocimiento Unported 3.0]]"))
	(t (insert "* License
This page is under a [[http://creativecommons.org/licenses/by-sa/3.0/deed][Creative Commons Reconocimiento Unported 3.0]]")))
  (insert "[[file:http://i.creativecommons.org/l/by-sa/3.0/88x31.png]]"))

(defun org-license-cc-by-nd (language)
  (interactive "MLanguage: " language)
  (cond ((equal language "es")
	 (insert "* Licencia
Esta página está bajo una [[http://creativecommons.org/licenses/by-nd/3.0/es/deed][Licencia Creative Commons
Atribución-SinDerivadas 3.0]]"))
	(t (insert "* License
This page is under a [[http://creativecommons.org/licenses/by-nd/3.0/deed][Creative Commons Reconocimiento Unported 3.0]]")))
  (insert "[[file:http://i.creativecommons.org/l/by-nd/3.0/88x31.png]]"))
;;"))

(defun org-license-cc-by-nc (language)
  (interactive "MLanguage: " language)
  (cond ((equal language "es")
	 (insert "* Licencia
Esta página está bajo una [[http://creativecommons.org/licenses/by-nc/3.0/es/deed][Licencia Creative Commons
Reconocimiento-NoComercial 3.0]]"))
	(t (insert "* License 
This page is under a [[http://creativecommons.org/licenses/by-nc/3.0/deed][Creative Commons Attribution-NonCommercial 3.0 Unported]]")))
  (insert "[[file:http://i.creativecommons.org/l/by-nc/3.0/88x31.png]]"))


(defun org-license-cc-by-nc-sa (language)
  (interactive "MLanguage: " language)
  (cond ((equal language "es")
	 (insert "* Licencia
Esta página está bajo una [[http://creativecommons.org/licenses/by-nc-sa/3.0/es/deed][Licencia Creative Commons Reconocimiento-NoComercial 3.0]]"))
	(t (insert "* License
This page is under a [[http://creativecommons.org/licenses/by-nc-sa/3.0/deed][License Creative Commons
Reconocimiento-NoComercial 3.0] Unported]")))
  (insert "[[file:http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png]]"))

(defun org-license-cc-by-nc-nd (language)
  (interactive "MLanguage: " language)
  (cond ((equal language "es")
	 (insert "* Licencia 
Esta página está bajo una [[http://creativecommons.org/licenses/by-nc-nd/3.0/deed][Licencia Creative Commons
Reconocimiento-NoComercial-SinObraDerivada 3.0 Unported]]"))
	(t (insert "* License
This page is under a [[http://creativecommons.org/licenses/by-nc-sa/3.0/deed][License Creative Commons
Reconocimiento-NoComercial-SinObraDerivada 3.0] Unported]")))
  (insert "[[file:http://i.creativecommons.org/l/by-nc-nd/3.0/88x31.png]]"))

