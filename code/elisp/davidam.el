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

(defun davidam-org-envolve-numbered-list()
  "Itemize some lines as a numbered list"
  (interactive)
  (setq num 1)
  (setq max (+ 1 (count-lines (point) (mark))))
  (if (> (point) (mark))
      (goto-line (+ 1 (count-lines 1 (mark))))
    (goto-line (+ 1 (count-lines 1 (point)))))
  (while (< num max)
    (move-beginning-of-line nil)
    (insert (concat (number-to-string num) ". "))
    (setq num (+ 1 num))
    (forward-line)))

(defun davidam-org-envolve-check-list()
  "Itemize some lines as a checked list"
  (interactive)
  (setq num 1)
  (setq max (+ 1 (count-lines (point) (mark))))
  (if (> (point) (mark))
      (goto-line (+ 1 (count-lines 1 (mark))))
    (goto-line (+ 1 (count-lines 1 (point)))))
  (while (< num max)
    (move-beginning-of-line nil)
    (insert (concat "+ [ ] "))
    (setq num (+ 1 num))
    (forward-line)))

(defun davidam-org-envolve-src(msg)
  "Envolve source between org tags"
  (interactive "sChoose your programming language: " msg)
  (if (string= "" msg)
      (setq msg "lisp"))
  (if (> (point) (mark))
      (progn 
	(goto-char (point))
	(insert "#+END_SRC")
	(goto-char (mark))
	(insert "#+BEGIN_SRC " msg "\n"))
    (progn
      (goto-char (point))
      (insert "#+BEGIN_SRC " msg "\n")
      (goto-char (mark))
      (insert "#+END_SRC"))))

(defun davidam-xml-envolve(tag)
  "Envolve source between xml tags"
  (interactive "sChoose your tag: " tag)
  (if (string= "" tag)
      (message "You must write a tag")
    (if (> (point) (mark))
	(progn 
	  (goto-char (point))
	  (insert (concat "</" tag ">"))
	  (goto-char (mark))
	  (insert (concat "<" tag ">")))
      (progn
	(goto-char (point))
	(insert (concat "<" tag ">"))
	(goto-char (mark))
	(insert (concat "</" tag ">"))))))


(defun davidam-org-src(msg)
  "Insert org source tags"
  (interactive "sChoose your programming language: " msg)
;; TODO: Meter name en interactive. Ej: #+name: myconcat
  (if (equal nil msg) 
      (setq msg "lisp"))
  (insert "#+BEGIN_SRC " msg)
  (insert "\n#+END_SRC\n"))

(defun davidam-org-display-date ()
  (interactive)
  (setq item-time (org-get-scheduled-time (point)))
  (message "%s" item-time))

(define-skeleton davidam-org-bibliography
  "A skeleton to bibliography for org-mode"
  ""
  '(setq author (skeleton-read "Author (Surname, Initials): ")) \n
  '(setq year (skeleton-read "Year: ")) \n 
  '(setq title (skeleton-read "Title: ")) \n
  '(setq publisher (skeleton-read "Publisher: ")) \n
  '(setq pages (skeleton-read "Pages Number: ")) \n
  > "+ " author " (" year "). " "\"/" title "/\" " publisher ". pp " pages "." \n
)

(define-skeleton davidam-org-bibliography-url
  "A skeleton to bibliography for org-mode"
  ""
  '(setq author (skeleton-read "Author (Surname, Initials): ")) \n
  '(setq year (skeleton-read "Year: ")) \n 
  '(setq date-seen (skeleton-read "Date seen: ")) \n 
  '(setq title (skeleton-read "Title: ")) \n
  '(setq publisher (skeleton-read "Publisher: ")) \n
  '(setq url (skeleton-read "Url: ")) \n
  > "+ " author " (" year "). " "\"/" title "/\" " publisher ". Visto el " date-seen " <" url  ">." \n
)

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

(defun davidam-org-todo-subtree (&optional ARG)
  "Change the state, such as org-todo, but for all the subtree"
  (interactive "P")
  (org-with-limited-levels (org-todo "DONE")))

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


