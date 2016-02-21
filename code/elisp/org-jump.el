;;; org-jump.el --- navigate over the structure of an org file
;;
;; Author: Christoph Lange <math.semantic.web@gmail.com>
;;
;; Copyright (C) 2016 by Christoph Lange 
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defun org-jump-to-child ()
  "Interactively prompts for the title of a child node of the current heading and jumps to the last child node having that title.  This function will not work if the first child node is not exactly one level below the current heading."
  (interactive)
  (let* ((child-position
          (save-excursion
            ;; Go to the heading of the current node
            (org-back-to-heading)
            (let* ((ref-level (org-reduced-level (org-outline-level))))
              (outline-next-heading)
              ;; If the next visible heading is one level lower, then ...
              (if (= (- (org-reduced-level (org-outline-level)) ref-level) 1)
                  (let* ((children nil)
                         (continue t))
                    (while continue
                      (let* ((old-point (point)))
                        ;; ... continue adding its headline text and the point to a list
                        (add-to-list 'children
                                     (cons
                                      ;; remove any markup such as links (copied from org-clock-in)
                                      (replace-regexp-in-string
                                       "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1"
                                       (substring-no-properties (nth 4 (org-heading-components))))
                                      old-point))
                        ;; ... and go to the next sibling.
                        (org-forward-heading-same-level 1 t)
                        (setq continue (/= (point) old-point))))
                    ;; Prompt for a child headline text, ...
                    (let* ((child (org-icompleting-read "Headline text: " (mapcar 'car children))))
                      ;; ... and return its location (point).
                      (cdr (assoc child children))))
                ;; Otherwise return -1.
                -1)))))
    ;; Go to the desired heading and make it visible, or otherwise output an error message.
    (if (> child-position -1)
        (progn
          (goto-char child-position)
          (org-show-context))
      (message "No children."))))

(define-key org-mode-map (kbd "\C-coc") 'org-jump-to-child)

(defun org-jump-to-id ()
  "Interactively prompts for an identifier and searches for the first node in the current file that has this identifier as a CUSTOM_ID property."
  (interactive)
  (let* ((property "CUSTOM_ID")
         (custom-id (org-icompleting-read "CUSTOM_ID of entry: "
                                          (mapcar 'list (org-property-values property)))))
    ;; What will happen if there is more than one node with this CUSTOM_ID?
    ;; Alternative implementation:
    ;; (org-jump-to-first-node-with-property-value property custom-id)
    (org-link-search (concat "#" custom-id))))

(define-key org-mode-map (kbd "\C-coj") 'org-jump-to-id)

(defun org-jump-to-first-node-with-property-value (property value)
  (interactive)
  (goto-char
   (car
    (org-scan-tags 'point 
                   (cdr
                    (org-make-tags-matcher (format "%s=\"%s\"" property value))))))
  (org-show-context))

;; further idea: org-find-text-property-in-string
