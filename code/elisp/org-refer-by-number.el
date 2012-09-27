;;; org-refer-by-number.el --- Create and search numbers used as references

;; Copyright (C) 2011,2012 Free Software Foundation, Inc.

;; Author: Marc-Oliver Ihm <ihm@ferntreffer.de>
;; Keywords: hypermedia, matching
;; Requires: org
;; Download: http://orgmode.org/worg/code/elisp/org-refer-by-number.el
;; Version: 1.5.0

;;; License:

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Purpose:
;;
;;  Refer to things by reference numbers, especially if direct linking is
;;  not possible. These reference numbers are added to and kept within a
;;  table along with the timestamp of their creation.
;;
;;  These reference numbers may then be used to refer to things outside of
;;  or within Org. E.g. by writing them on a piece of paper or using them
;;  as part of a directory name or the heading of an Org node. Within Org
;;  you may then refer to these things by their reference number
;;  (e.g. "R153"); the numbers can be looked up and searched easily.
;;
;;  The whole functionality is available through the function
;;  `org-refer-by-number'; the necessary setup is described in the
;;  docstring of the variable `org-refer-by-number-id'.
;;
;;  org-refer-by-number.el is only a small add-on to Carsten Dominiks
;;  Org-mode, which must be installed as a prerequisite. See
;;  http://orgmode.org or elpa for Org-mode itself.
;;
;; Setup:
;;
;;  - Adjust these lines and add them to your .emacs:
;;
;;    (require 'org-refer-by-number)
;;    (setq org-refer-by-number-id "00e26bef-1929-4110-b8b4-7eb9c9ab1fd4")
;;    ;; Optionally assign a key
;;    (global-set-key (kbd "C-+") 'org-refer-by-number)
;;
;;  - Create an Org-mode node with a reference table
;;    as described in the documentation of `org-refer-by-number-id'
;;
;; Further reading:
;;
;;  For the necessary setup, see the variable `org-refer-by-number-id';
;;  for regular usage, see the function `org-refer-by-number'.
;;

;;; Change Log:

;;   [2012-09-22 Sa] Version 1.5.0:
;;   - New operation "sort" to sort a buffer or region by reference number
;;   - New operations "highlight" and "unhighlight" to mark references

;;   [2012-07-13 Fr] Version 1.4.0:
;;   - New operation "head" to find a headline with a reference number

;;   [2012-04-28 Sa] Version 1.3.0:
;;   - New operations occur and multi-occur
;;   - All operations can now be invoked explicitly
;;   - New documentation
;;   - Many bugfixes

;;   [2011-12-10 Sa] Version 1.2.0:
;;   - Fixed a bug, which lead to a loss of newly created reference numbers
;;   - Introduced single and double prefix arguments
;;   - Started this Change Log

;;; Code:

(require 'org-table)

(defvar org-refer-by-number-id nil 
  "Id of the Org-mode node, with the table of reference numbers.

Read below, on how to set up things. See the documentation of
`org-refer-by-number' for normal usage after setup.

Setup requires two steps:

- Create a suitable org-mode node
- Adjust your .emacs initialization file


Here is how you create the org-mode node, where your reference
numbers will be stored. It may look like this:


  * My node for org-refer-by-number
    :PROPERTIES:
    :ID:       00e26bef-1929-4110-b8b4-7eb9c9ab1fd4
    :END:
  
    | Number | Date            | Comment                    |
    |--------+-----------------+----------------------------|
    | R1     | [2012-04-28 Sa] | My first reference number  |


You may just want to copy this node into one of your org-files.
Many things however can or should be adjusted:

- The node needs not be a top level node.

- Its name is completely at you choice. The node is found
  through its ID.

- Column names can be changed.

- You can add further columns or even remove the
  \"Comment\"-column. The columns \"Number\" and \"Date\" however
  are required.

- Your references need not start at \"R1\"; and of course you can
  adjust date and comment.  However, having an initial row is
  required (it servers a template for subsequent references).

- Your reference need not have the form \"R1\"; you may just as
  well choose any text, that contains a single number,
  e.g. \"reference-{1}\" or \"#1\" or \"++1++\". The function
  `org-refer-by-number' will inspect your first reference and
  create all subsequent references in the same way.
    
- You may want to change the ID-Property of the node above and
  create a new one, which is unique (and not just a copy of
  mine). You need to change it in the lines copied to your .emacs
  too. However, this is not strictly required to make things
  work, so you may do this later, after trying out this package.


Having created the node with your reference table, you only need
to add some lines to your .emacs:

  (require 'org-refer-by-number)
  (setq org-refer-by-number-id \"00e26bef-1929-4110-b8b4-7eb9c9ab1fd4\")
  ;; Optionally assign a key
  (global-set-key (kbd \"C-+\") 'org-refer-by-number)

Do not forget to restart emacs to make these lines effective.


After this two-step setup you may invoke `org-refer-by-number' to
create a new reference number; read there for instructions on
normal usage.

")

(defvar org-refer-by-number-windowconfig nil 
  "Saved window-configuration for `org-refer-by-number'.
This variable is only used internally.")

(defvar org-refer-by-number-marker nil 
  "Saved marker for `org-refer-by-number'.
This variable is only used internally.")

(defvar org-refer-by-number-last-action nil 
  "Last action performed by `org-refer-by-number'.
This variable is only used internally.")

(defvar org-refer-by-number-occur-buffer nil
  "Buffer (if any) with result from occur or multi-occur.
This variable is only used internally.")

(defun org-refer-by-number (arg) 
  "Add a new reference number or search for an existing one.

These reference numbers may then be used to refer to things
outside of Org in cases, where direct linking is not
possible. E.g. you may write them on documents or letters you
receive or use them on your computer as part of foldernames that
you create.

Read below for a detailed description of this function. See the
documentation of `org-refer-by-number-id' for the necessary
setup.


The function `org-refer-by-number' operates on a dedicated
table (called the reference table) within a special Org-mode
node. The node has to be created as part of your initial
setup. The reference table has at least two columns: The
reference number (automatically increasing by one from row to
row) and the date of creation. The table is found through the id
of the containing node; this id must be stored within
`org-refer-by-number-id' (see there for details).


The function `org-refer-by-number' is the only interactive
function of this package and its sole entry point; it offers seven
different operations (short names in parens):

- Add a new row with a new reference number and the
  date of creation (\"add\").

- Search for an existing reference number within your reference
  table (\"search\").

- Find all occurences of a particular string within your
  reference table; typically within the comment
  column (\"occur\").

- Find all occurences of a particular reference number within all
  of your org-files (\"multi-occur\").

- Go to the first heading, that contains a given reference
  number (\"heading\").

- Enter the reference table and position the cursor at the
  top (\"enter\").

- Leave the reference table and restore cursor position and
  window configuration, back to the state before entering
  it (\"leave\").

- Sort lines in current buffer or active region by the first
  reference number, they contain, if any (\"sort\").

- Highlight or unhighlight all occurences of reference number
  within current buffer (\"highlight\" or \"unhighlight\").
 
The most straightforward way to select between these operations
is to supply a negative or a a double prefix argument:

`C-- \\[org-refer-by-number]' or `\\[universal-argument] \\[universal-argument] \\[org-refer-by-number]'

You will then be prompted to type a single letter (\"a\", \"s\",
\"o\", \"m\", \"h\", \"e\" or \"l\") to invoke the respective
operation from the list above. If your type \"+\" you will be
prompted a second time to choose among some of the less common
operations (e.g. \"sort\").


Some of the operations above can be invoked with less keystrokes. In that
case the precise operation invoked depends on two things:

- The kind of a prefix argument (or absence of such)

- The location of point; either outside the reference table or
  within


The following cases explain, which of the seven
operations (\"add\", \"search\", \"occur\", \"multi-occur\",
\"heading\", \"enter\" and \"leave\") is actually invoked
depending on the conditions above:


  If no prefix argument is given (`\\[org-refer-by-number]') and
  point is withib the reference table, the operation \"leave\"
  will be invoked. If point is within reference table, a
  \"search\" will be done in most cases; if, however, there is an
  active region, the operation \"add\" is performed.

  If a numeric prefix argument is given (e.g. `153 \\[org-refer-by-number]'): 
  The function does a \"search\" for this reference number, 
  if point is outside and a \"multi-occur\", if point is within
  regardless of position of point.

  If a single prefix argument is given (e.g. `\\[universal-argument] \\[org-refer-by-number]')
  and point outside the reference table: \"add\" a new reference. 
  If point within: Do a \"multi-occur\" for the given reference.


In any case the function `org-refer-by-number' will give a short
message to explain, what operation has actually been invoked.


Before you can successfully use `org-refer-by-number' finally,
you need to read the documentation of `org-refer-by-number-id'
and complete the necessary setup decribed there.


"

  (interactive "P")

  (let (within-node        ; True, if we are within node with reference table
                           ; (false otherwise, even if we are in the
                           ; right buffer)
        result-is-visible  ; True, if node or occur is visible in any window
        search-from-prefix ; search string from prefix-argument
        search-from-table  ; search string from first column of table
        search-from-cursor ; search string from text below cursro
        search-from-user   ; search string from user input
        below-cursor       ; word below cursor
        active-region      ; active region (if any)
        search             ; final string to search for
        guarded-search     ; with guard against additional digits
        what               ; What are we supposed to do ? Will be stored in
                           ; org-refer-by-number-last-action
        what-adjusted      ; True, if we had to adjust what
        what-explicit      ; True, if what has been specified explicitly
        parts              ; Parts of a typical reference number (which
                           ; need not be a plain number); these are:
        head               ; Any header before number (e.g. "R")
        last-number        ; Last number used in reference table (e.g. "153")
        tail               ; Tail after number (e.g. "}" or "")
        ref-regex          ; Regular expression to match a reference
        columns            ; Number of columns in reference table
        kill-new-text      ; Text that will be appended to kill ring
        message-text       ; Text that will be issued as an explanation,
                           ; what we have done
        node-marker        ; Initial point within buffer with reference table
        )
        
    ;; Find out, if we are within reference table or not
    (setq within-node (string= (org-id-get) org-refer-by-number-id))
    ;; Find out, if point in any window is within node with reference table
    (mapc (lambda (x) (save-excursion 
                        (set-buffer (window-buffer x))
                        (when (or 
                               (string= (org-id-get) org-refer-by-number-id)
                               (eq (window-buffer x) 
                                   org-refer-by-number-occur-buffer))
                          (setq result-is-visible t))))
          (window-list))

    ;; Get the content of the active region or the word under cursor; do
    ;; this before examinig reference table
     (if (and transient-mark-mode
              mark-active)
        (setq active-region (buffer-substring (region-beginning) (region-end))))
     (setq below-cursor (thing-at-point 'symbol))

    ;; Find out, what we are supposed to do
    (cond ((equal arg nil)
           (setq what (if result-is-visible 'leave 
                        (if active-region 'add 'search))))
          ((equal arg '(4))
           (setq what (if within-node 'multi-occur 'add)))
          ((numberp arg)
           (setq what (if within-node 'multi-occur 'search)))
          (t ; C-- or C-u C-u
           (let (key)
             (while 
                 (progn 
                   (setq key (read-char-exclusive 
                              "Please choose: e=enter l=leave s=search a=add o=occur m=multi-occur h=heading +=more choices"))
                   (not 
                    (setq what (case key
                                 (?l 'leave) (?e 'enter) (?a 'add) 
                                 (?s 'search) (?o 'occur) (?m 'multi-occur) (?h 'heading) (?+ 'more)))))
               (message "Invalid key '%c'" key)
               (sleep-for 1))
             (if (eq what 'more)
                 (setq what (cdr (assoc 
                                  (org-icompleting-read "Please choose: " '("sort" "highlight" "unhighlight") nil t)
                                  '(("sort" . sort)("highlight" . highlight)("unhighlight" . unhighlight))))))
             (setq what-explicit t))))

    ;; Get decoration and number of last row from reference table
    (let ((m (org-id-find org-refer-by-number-id 'marker)))
      (unless m
        (org-refer-by-number-report-setup-error 
         (format "Cannot find node with id \"%s\"" org-refer-by-number-id)))
      (with-current-buffer (marker-buffer m)
        (setq node-marker (point-marker))
        (setq node-buffer (marker-buffer node-marker))
        (goto-char m)
        (setq parts (org-refer-by-number-trim-table nil t))
        (goto-char node-marker))
      (move-marker m nil)
      (setq head (nth 0 parts))
      (setq last-number (nth 1 parts))
      (setq tail (nth 2 parts))
      (setq columns (nth 3 parts))
      (setq ref-regex (concat (regexp-quote head) "\\([0-9]+\\)" (regexp-quote tail))))
      

    ;; These actions need a search string:
    (when (memq what '(search occur multi-occur heading))

      ;; Search string can come from several sources:
      ;; From explicit numerical prefix
      (if (numberp arg) 
          (setq search-from-prefix (format "%s%d%s" head arg tail)))
      ;; From first column of table
      (when within-node
        (save-excursion (setq search-from-table (org-table-get-field 1)))
        (if (string= search-from-table "") (setq search-from-table nil)))      
      ;; From string below cursor
      (when (and (not within-node)
                 below-cursor
                 (string-match (concat "\\(" ref-regex "\\)") 
                               below-cursor))
        (setq search-from-cursor (match-string 1 below-cursor)))
      
      ;; Depending on requested action, get search from one of the sources above
      (cond ((eq what 'search)
             (setq search (or search-from-prefix search-from-cursor)))
            ((or (eq what 'multi-occur) (eq what 'heading))
             (setq search (or search-from-table search-from-cursor)))
            ((eq what 'occur)
             (setq search active-region)))


      ;; If we still do not have a search string, ask user explicitly
      (unless search
        (setq search (read-from-minibuffer
                      (cond ((memq what '(search multi-occur heading))
                             "Reference number to search for: ")
                            ((eq what 'occur)
                             "Text to search for: "))))
        (if (string-match "^\\s *[0-9]*\\s *$" search)
            (unless (string= search "")
              (setq search (format "%s%s%s" head (org-trim search) tail)))))
      
      ;; Clean up search string
      (if (string= search "") (setq search nil))
      (if search (setq search (org-trim search)))

      (setq guarded-search 
            (concat (regexp-quote search)
                    ;; if there is no tail in reference number, we
                    ;; have to guard agains trailing digits
                    (if (string= tail "") "\\($\\|[^0-9]\\)" "")))

    
      ;; Correct requested action, if nothing to search
      (when (and (not search)
               (memq what '(search occur multi-occur heading)))
          (setq what 'enter)
          (setq what-adjusted t))
           
      ;; Check for invalid combinations of arguments; try to be helpful
      (if (string-match ref-regex search)
          (if (eq what 'occur) 
              (error "Can do 'occur' only for text, try 'search', 'multi-occur' or 'heading' for a number"))
        (if (memq what '(search multi-occur heading))
            (error "Can do '%s' only for a number, try 'occur' to search for text" what))))
    
    ;; Move into table, if outside ...
    (when (memq what '(enter add search occur multi-occur))
      ;; Save current window configuration
      (when (or (not result-is-visible)
                (not org-refer-by-number-windowconfig))
        (setq org-refer-by-number-windowconfig (current-window-configuration))
        (setq org-refer-by-number-marker node-marker))
      
      ;; Switch to reference table; this needs to duplicate some code from
      ;; org-id-goto, because point should be moved, if what equals 'enter
      (let ((m (org-id-find org-refer-by-number-id 'marker)))
        (org-pop-to-buffer-same-window (marker-buffer m))
        ;; After changing buffer we might be in table or not, so check again
        (setq within-node (string= (org-id-get) org-refer-by-number-id))
        ;; Be careful with position within table, if we should just enter it
        (unless within-node (goto-char m))
        (move-marker m nil)
        (show-subtree)
        (org-show-context)))


    ;; Actually do, what is requested
    (cond
     ((eq what 'multi-occur) 
      
      ;; Position cursor on number to search for
      (org-refer-by-number-trim-table t)
      (let (found (initial (point)))
        (forward-line)
        (while (and (not found)
                    (forward-line -1)
                    (org-at-table-p))
          (save-excursion 
            (setq found (string= search 
                                 (org-trim (org-table-get-field 1))))))
        (if found 
            (org-table-goto-column 1)
          (goto-char initial)))

      ;; Construct list of all org-buffers
      (let (buff org-buffers)
        (dolist (buff (buffer-list))
          (set-buffer buff)
          (if (string= major-mode "org-mode")
              (setq org-buffers (cons buff org-buffers))))
       
        ;; Do multi-occur
        (multi-occur org-buffers guarded-search)
        (if (get-buffer "*Occur*")
            (progn 
              (setq message-text (format "multi-occur for '%s'" search))
              (setq org-refer-by-number-occur-buffer (get-buffer "*Occur*")))
          (setq message-text (format "Did not find '%s'" search)))))


     ((eq what 'heading)
      (message (format "Scanning headlines for '%s' ..." search))
      (let (buffer point)
        (if (catch 'found
              (progn
                (org-map-entries 
                 (lambda () 
                   (when (looking-at (concat ".*\\b" guarded-search))
                     (setq buffer (current-buffer))
                     (setq point (point))
                     (throw 'found t)))          
                 nil 'agenda)
                nil))
            (progn
              (setq message-text (format "Found '%s'" search))
              (org-pop-to-buffer-same-window buffer)
              (goto-char point)
              (org-reveal))
          (setq message-text (format "Did not find '%s'" search)))))


     ((eq what 'leave)

      (when result-is-visible

        ;; if we are within the occur-buffer, switch over to get current line
        (if (and (string= (buffer-name) "*Occur*")
                 (eq org-refer-by-number-last-action 'occur))
            (occur-mode-goto-occurrence))
        
        (if (org-at-table-p)
            (let ((column (org-table-current-column)))
              ;; Copy different things depending on the last action
              (if (and (eq org-refer-by-number-last-action 'search)
                       (= column 1))
                  ;; It does not help to copy the first field, because
                  ;; thats what we just searched for, so take last one
                  (setq column columns))
              (if (or (memq org-refer-by-number-last-action '(add occur))
                      (< column 1))
                  (setq column 1))
              
              ;; Add to kill ring
              (if (memq org-refer-by-number-last-action '(add enter search occur))
                  ;; Got left to first nonempty column
                  (while (progn 
                           (save-excursion 
                             (setq kill-new-text 
                                   (org-trim (org-table-get-field column))))
                           (and (> column 0)
                                (string= kill-new-text "")))
                    (setq column (- column 1))))))
        
        ;; Clean up table before leaving
        (with-current-buffer node-buffer 
          (org-refer-by-number-trim-table t)
          (let ((buffer-modified (buffer-modified-p)))
            (org-table-align)
            (set-buffer-modified-p buffer-modified))))

      ;; Restore position within buffer with reference table
      (if org-refer-by-number-windowconfig 
          (progn  
            (with-current-buffer node-buffer
              (goto-char org-refer-by-number-marker)
              (set-marker org-refer-by-number-marker nil))
            ;; Restore initial window configuration
            (set-window-configuration org-refer-by-number-windowconfig)
            (setq org-refer-by-number-windowconfig nil)
            (recenter)
            (setq message-text "Back"))
        ;; We did not have a window-configuration to restore, so we cannot
        ;; pretend we have retturned back
        (setq message-text "Cannot leave; nowhere to go to")))

        
     ((eq what 'search)
      ;; Go upward in table within first column
      (org-refer-by-number-trim-table t)
      (let (found (initial (point)))
        (forward-line)
        (while (and (not found)
                    (forward-line -1)
                    (org-at-table-p))
          (save-excursion 
            (setq found 
                  (string= search 
                           (org-trim (org-table-get-field 1))))))
        (if found
            (progn
              (setq message-text (format "Found '%s'" search))
              (org-table-goto-column 1)
              (if (looking-back " ") (backward-char)))
          (setq message-text (format "Did not find '%s'" search))
          (goto-char initial)
          (forward-line)
          (setq what 'missed))))


     ((eq what 'occur)
      ;; search for string: occur
      (org-narrow-to-subtree)
      (occur search)
      (widen)
      (if (get-buffer "*Occur*")
          (progn
            (put 'org-refer-by-number 'occur-buffer (current-buffer))
            (other-window 1)
            (toggle-truncate-lines 1)
            (forward-line 1)
            (occur-mode-display-occurrence)
            (setq message-text
                  (format  "Occur for '%s'" search)))
        (setq message-text
              (format "Did not find any matches for '%s'" search))))

        
     ((eq what 'add)
      ;; Nothing to search for, add a new row
      (org-refer-by-number-trim-table t)
      (let ((new (format "%s%d%s" head (1+ last-number) tail)))
        (org-table-insert-row 1)
        (insert new)
        (org-table-goto-column 2)
        (org-insert-time-stamp nil nil t)
        (org-table-goto-column 3)
        (org-table-align)
        (if active-region (setq kill-new-text active-region))
        (setq message-text (format "Adding a new row '%s'" new))))
     
     
     ((eq what 'enter)
      ;; Already there, not much to do left
      (show-subtree)
      (recenter)
      (if what-adjusted
          (setq message-text "Nothing to search for; at reference table")
        (setq message-text "At reference table")))
     

     ((eq what 'sort)
      (let (begin end where)
        (if (if (and transient-mark-mode
                     mark-active)
                (progn
                  (setq begin (region-beginning))
                  (setq end (region-end))
                  (setq where "region")
                  t)
              (setq begin (point-min))
              (setq end (point-max))
              (setq where "whole buffer")
              (y-or-n-p "Sort whole buffer ")
              )
            (save-excursion
              (save-restriction
                (beginning-of-buffer)
                (narrow-to-region begin end)
                (sort-subr nil 'forward-line 'end-of-line 
                           (lambda ()
                             (if (looking-at (concat "^.*\\b" ref-regex "\\b"))
                                 (string-to-number (match-string 1))
                               0))))
              (highlight-regexp ref-regex)
              (setq message-text (format "Sorted %s from character %d to %d, %d lines" 
                                         where begin end
                                         (count-lines begin end))))
          (setq message-text "Sort aborted"))))
      

     ((memq what '(highlight unhighlight))
      (let ((where "buffer"))
        (save-excursion
          (save-restriction
            (when (and transient-mark-mode
                     mark-active)
                (narrow-to-region (region-beginning) (region-end))
                (setq where "region")
              )
          (if (eq what 'highlight)
              (progn
                (highlight-regexp ref-regex)
                (setq message-text (format "Highlighted references in %s" where)))
            (unhighlight-regexp ref-regex)
            (setq message-text (format "Removed highlights for references in %s" where)))))))
     

     (t (error "This is a bug: Unmatched condition '%s'" what)))

    
    ;; Remember what we have done for next time
    (setq org-refer-by-number-last-action what)
    
    ;; Tell, what we have done and what can be yanked
    (if kill-new-text (setq kill-new-text 
                            (substring-no-properties kill-new-text)))
    (if (string= kill-new-text "") (setq kill-new-text nil))
    (let ((m (concat 
              message-text
              (if (and message-text kill-new-text) 
                  " and r" 
                (if kill-new-text "R" ""))
              (if kill-new-text (format "eady to yank '%s'" kill-new-text) "")
              )))
      (unless (string= m "") (message m)))
    (if kill-new-text (kill-new kill-new-text))))


(defun org-refer-by-number-trim-table (&optional goto-end get-parts)
  "Trim reference table, only used internally"
  
  (let ((initial (point-marker))
        field
        parts
        columns)

    ;; Go to heading of node
    (while (not (org-at-heading-p)) (forward-line -1))
    (forward-line 1)
    ;; Go to table within node, but make sure we do not get into another node
    (while (and (not (org-at-heading-p))
                (not (org-at-table-p))
                (not (eq (point) (point-max)))) 
      (forward-line 1))
    ;; Check, if there really is a table
    (unless (org-at-table-p)
      (org-refer-by-number-report-setup-error 
       "Cannot find reference table within reference node" t))

    ;; Go beyond end of table
    (while (org-at-table-p) (forward-line 1))

    ;; Kill all empty rows at bottom
    (while (progn
             (forward-line -1)
             (org-table-goto-column 1)
             (string= "" (org-trim (org-table-get-field 1)))
             )
      (org-table-kill-row)
      )

    (when get-parts

      ;; Find out number of columns
      (org-table-goto-column 100)
      (setq columns (- (org-table-current-column) 1))

      ;; Check for right number of columns
      (unless (>= columns 2)
        (org-refer-by-number-report-setup-error 
         "Table within reference node has less than two columns" t)
        )

      ;; Retrieve any decorations around the number within first field of
      ;; the last row
      (setq field (org-trim (org-table-get-field 1)))
      (or (string-match "^\\([^0-9]*\\)\\([0-9]+\\)\\([^0-9]*\\)$" field)
          (org-refer-by-number-report-setup-error 
           (format "Last field of reference table '%s' does not contain a number" field) t)
          )

      ;; These are the decorations used within the last row of the
      ;; reference table
      (setq parts (list (match-string 1 field) 
                        (string-to-number (match-string 2 field)) 
                        (match-string 3 field) 
                        columns)))

    (unless goto-end (goto-char (marker-position initial)))
    (set-marker initial nil)
    
    parts))


(defun org-refer-by-number-report-setup-error (text &optional switch-to-node)
  "Report error, which might be related with incomplete setup; offer help"

  (when switch-to-node 
    (org-id-goto org-refer-by-number-id)
    (delete-other-windows)
    )
  
  (if (y-or-n-p (concat text 
                        "; "
                        "the correct setup is explained in the documentation of 'org-refer-by-number-id'. " 
                        "Do you want to read it ? "))
      (describe-variable 'org-refer-by-number-id)
    )
  (error "")
  (setq org-refer-by-number-windowconfig nil)
  (setq org-refer-by-number-last-action 'leave))


(provide 'org-refer-by-number)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; org-refer-by-number.el ends here

