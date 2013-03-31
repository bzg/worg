;; Copyright (C) 2013  David Arroyo Menéndez

;; Author: David Arroyo Menéndez <davidam@es.gnu.org>
;; Maintainer: David Arroyo Menéndez <davidam@es.gnu.org>

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
;; Boston, MA 02110-1301 USA,


(defun davidam-org-envolve-src(msg)
  "Envolve source between org tags"
  (interactive "sChoose your programming language: " msg)
  (if (equal nil msg) 
      (setq msg "lisp"))
  (goto-char (point))
  (insert "\n#+END_SRC")
  (goto-char (mark))
  (insert "#+BEGIN_SRC " msg))

(defun davidam-org-src(msg)
  "Insert org source tags"
  (interactive "sChoose your programming language: " msg)
  (if (equal nil msg) 
      (setq msg "lisp"))
  (insert "#+BEGIN_SRC " msg)
  (insert "\n#+END_SRC\n"))

(defun davidam-happy-birthday(person)
  (interactive "MWrite the person name: ")
  (message (concat "Happy birthday to you. Happy birthday to you. Happy birthday, dear " person ". Happy birthday to you")))

(defun davidam-fibonacci (n)
  (interactive "nEscribe un numero: " n)
  (message (number-to-string (fibonacci-aux n))))

(defun fibonacci-aux (n)
;;  (interactive "dEscribe un numero: " n)
  (if (or (= n 0) (= n 1))
      1
    (+ (fibonacci-aux (- n 1)) (fibonacci-aux (- n 2)))))

(defun davidam-torres-de-hanoi (discos)
  (interactive "nDime tus discos y te digo cuantos pasos tienes que dar: " discos)
  (message (number-to-string (torres-de-hanoi-aux discos))))

(defun torres-de-hanoi-aux (discos)
  (if (= discos 1)
      1
    (+ 1 (* 2 (torres-de-hanoi-aux (- discos 1))))))

(defun davidam-insert-output (command)
   (interactive "sCommand: ")
   (insert (shell-command-to-string command)))

(defun davidam-output-to-buffer (buffer command)
   (interactive "sBuffer name: \nsCommand: ")
   (get-buffer-create buffer)
   (call-process command nil buffer)
   (switch-to-buffer (get-buffer buffer)))

(defun davidam-rsync-rmail ()
  (interactive)
  (get-buffer-create "rsync")
  (call-process "/home/davidam/scripts/rsync-rmail.sh" nil "rsync")
  (switch-to-buffer (get-buffer "rsync")))

(defun davidam-multiply-two (number1 number2)
  (interactive "nNumber 1: \nnNumber 2: " number1 number2)
  (message "The result is %d" (* number1 number2)))

(defun davidam-triangle-using-dotimes (number-of-rows)
  "Using dotimes, add up the number of pebbles in a triangle."
  (interactive "nNumber of rows: " number-of-rows) 
  (let ((total 0))  ; otherwise a total is a void variable
    (dotimes (number number-of-rows total)
      (setq total (+ total (1+ number))))
    (message "Number of pebbles: %d" total)))
