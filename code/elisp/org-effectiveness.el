;;; org-effectiveness.el --- Measuring the personal effectiveness

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Author: David Arroyo Menéndez <davidam@es.gnu.org>
;; Keywords: effectiveness, plot
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

;; This file implements functions to measure the effectiveness in org.
;; Org-mode doesn't load this module by default - if this is not what
;; you want, configure the variable `org-modules'. Thanks to #emacs-es
;; irc channel for your support.

;;; Code:

(require 'org)

(defun org-count-keyword(keyword)
  "Print a message with the number of keyword outline in the current buffer"
  (interactive "sKeyword: ")
  (save-excursion
    (goto-char (point-min))
    (message "Number of %s: %d" keyword (count-matches (concat "* " keyword)))))

(defun org-count-todo()
  "Print a message with the number of todo tasks in the current buffer"
  (interactive)
  (save-excursion 
    (goto-char (point-min))
    (message "Number of TODO: %d" (count-matches "* TODO"))))
									     
(defun org-count-done()
  "Print a message with the number of done tasks in the current buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (message "Number of DONE: %d" (count-matches "* DONE"))))

(defun org-count-canceled()
  "Print a message with the number of canceled tasks in the current buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (message "Number of Canceled: %d" (count-matches "* CANCELED"))))

(defun org-effectiveness()
  "Returns the effectiveness in the current org buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((done (float (count-matches "* DONE.*\n.*")))
	  (canc (float (count-matches "* CANCELED.*\n.*"))))
      (if (and (= done canc) (zerop done))
	  (setq effectiveness 0)
	(setq effectiveness (* 100 (/ done (+ done canc)))))
      (message "Effectiveness: %f" effectiveness))))

(defun org-keywords-in-date(keyword date)
  (interactive "sKeyword: \nsDate: " keyword date)
  (setq count (count-matches (concat keyword ".*\n.*" date)))
  (message (concat "%sS: %d" keyword count)))

(defun org-dones-in-date(date)
   (interactive "sGive me a date: " date)
   (setq count (count-matches (concat "DONE.*\n.*" date)))
   (message "DONES: %d" count))

(defun org-todos-in-date(date)
   (interactive "sGive me a date: " date)
   (setq count (count-matches (concat "TODO.*\n.*" date)))
   (message "TODOS: %d" count))

(defun org-canceled-in-date(date)
   (interactive "sGive me a date: " date)
   (setq count (count-matches (concat "TODO.*\n.*" date)))
   (message "CANCELEDS: %d" count))

(defun org-effectiveness-in-date(date)
  (interactive "sGive me a date: " date)
  (save-excursion
    (goto-char (point-min))
    (let ((done (float (count-matches (concat "* DONE.*\n.*" date))))
	  (canc (float (count-matches (concat "* CANCELED.*\n.*" date)))))
      (if (and (= done canc) (zerop done))
	  (setq effectiveness 0)
	(setq effectiveness (* 100 (/ done (+ done canc)))))
      (message "Effectiveness: %d " effectiveness))))

(provide 'org-effectiveness)
