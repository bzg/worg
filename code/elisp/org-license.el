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
Este documento está bajo una [[http://creativecommons.org/licenses/by/3.0/deed][Licencia Creative Commons Reconocimiento Unported 3.0]]"))
	(t (insert "* License
This document is under a [[http://creativecommons.org/licenses/by/3.0/deed][Creative Commons Attribution Unported 3.0]]")))
  (insert "\n\n[[file:http://i.creativecommons.org/l/by/3.0/88x31.png]]"))

(defun org-license-cc-by-sa (language)
  (interactive "MLanguage: " language)
  (cond ((equal language "es") 
	 (insert "* Licencia
Este documento está bajo una [[http://creativecommons.org/licenses/by-sa/3.0/deed][Licencia Creative Commons Reconocimiento Unported 3.0]]"))
	(t (insert "* License
This document is under a [[http://creativecommons.org/licenses/by-sa/3.0/deed][Creative Commons Reconocimiento Unported 3.0]]")))
  (insert "\n\n[[file:http://i.creativecommons.org/l/by-sa/3.0/88x31.png]]"))

(defun org-license-cc-by-nd (language)
  (interactive "MLanguage: " language)
  (cond ((equal language "es")
	 (insert "* Licencia
Este documento está bajo una [[http://creativecommons.org/licenses/by-nd/3.0/es/deed][Licencia Creative Commons
Atribución-SinDerivadas 3.0]]"))
	(t (insert "* License
This document is under a [[http://creativecommons.org/licenses/by-nd/3.0/deed][Creative Commons Reconocimiento Unported 3.0]]")))
  (insert "\n\n[[file:http://i.creativecommons.org/l/by-nd/3.0/88x31.png]]"))
;;"))

(defun org-license-cc-by-nc (language)
  (interactive "MLanguage: " language)
  (cond ((equal language "es")
	 (insert "* Licencia
Este documento está bajo una [[http://creativecommons.org/licenses/by-nc/3.0/es/deed][Licencia Creative Commons
Reconocimiento-NoComercial 3.0]]"))
	(t (insert "* License 
This document is under a [[http://creativecommons.org/licenses/by-nc/3.0/deed][Creative Commons Attribution-NonCommercial 3.0 Unported]]")))
  (insert "\n\n[[file:http://i.creativecommons.org/l/by-nc/3.0/88x31.png]]"))


(defun org-license-cc-by-nc-sa (language)
  (interactive "MLanguage: " language)
  (cond ((equal language "es")
	 (insert "* Licencia
Este documento está bajo una [[http://creativecommons.org/licenses/by-nc-sa/3.0/es/deed][Licencia Creative Commons Reconocimiento-NoComercial 3.0]]"))
	(t (insert "* License
This document is under a [[http://creativecommons.org/licenses/by-nc-sa/3.0/deed][License Creative Commons
Reconocimiento-NoComercial 3.0] Unported]")))
  (insert "\n\n[[file:http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png]]"))

(defun org-license-cc-by-nc-nd (language)
  (interactive "MLanguage: " language)
  (cond ((equal language "es")
	 (insert "* Licencia 
Este documento está bajo una [[http://creativecommons.org/licenses/by-nc-nd/3.0/deed][Licencia Creative Commons
Reconocimiento-NoComercial-SinObraDerivada 3.0 Unported]]"))
	(t (insert "* License
This document is under a [[http://creativecommons.org/licenses/by-nc-sa/3.0/deed][License Creative Commons
Reconocimiento-NoComercial-SinObraDerivada 3.0] Unported]")))
  (insert "\n\n[[file:http://i.creativecommons.org/l/by-nc-nd/3.0/88x31.png]]"))

(defun org-license-gfdl (language)
  (interactive "MLanguage: " language)
  (cond ((equal language "es")
	 (insert "* Licencia
Copyright (C)  2013 " user-full-name
"\n    Se permite copiar, distribuir y/o modificar este documento
    bajo los términos de la GNU Free Documentation License, Version 1.3
    o cualquier versión publicada por la Free Software Foundation;
    sin Secciones Invariantes y sin Textos de Portada o Contraportada.
    Una copia de la licencia está incluida en [[https://www.gnu.org/copyleft/fdl.html][GNU Free Documentation License]]."))
	(t (insert (concat "* License
Copyright (C)  2013 " user-full-name
"\n    Permission is granted to copy, distribute and/or modify this document
    under the terms of the GNU Free Documentation License, Version 1.3
    or any later version published by the Free Software Foundation;
    with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.
    A copy of the license is included in [[https://www.gnu.org/copyleft/fdl.html][GNU Free Documentation License]]."))))
  (insert "\n\n[[file:https://upload.wikimedia.org/wikipedia/commons/thumb/4/42/GFDL_Logo.svg/200px-GFDL_Logo.svg.png]]"))
