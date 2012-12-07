;;; org-reftable.el --- Ordered lookup table for reference numbers

;; Copyright (C) 2011,2012 Free Software Foundation, Inc.

;; Author: Marc-Oliver Ihm <org-reftable@ferntreffer.de>
;; Keywords: hypermedia, matching
;; Requires: org
;; Download: http://orgmode.org/worg/code/elisp/org-reftable.el
;; Version: 2.0.0

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
;;  Create, search and look up numbers from a dedicated reference table.
;;  These numbers (e.g. "R237" or "-455-") may be used to refer to:
;;
;;   - Nodes in Org-mode (insert them into the heading)
;;
;;   - Things outside of org (e.g. mailfolders, directories, reports or
;;     pieces of paper)
;;
;;  The table is kept sorted for most frequently or most recently used
;;  reference numbers. Additionally, lines can be selected by keywords, so
;;  that specific references can be found very easily. Earlier versions of 
;;  this extension had been named org-refer-by-number.el.
;;
;;
;; Setup:
;;
;;  - Add these lines to your .emacs:
;;
;;    (require 'org-reftable)
;;    ;; Later you should probably change this id, as will be explained below
;;    (setq org-reftable-id "00e26bef-1929-4110-b8b4-7eb9c9ab1fd4")
;;    ;; Optionally assign a key; pick your own favorite
;;    (global-set-key (kbd "C-+") 'org-reftable)
;;
;;  - Just invoke `org-reftable', which will explain how to complete your
;;    setup by creating the necessary reference table.
;;
;;
;; Further reading:
;;
;;  - For the necessary setup read the documentation of `org-reftable-id'
;;    (which is, what `org-reftable' shows, as long as your setup is still
;;    incomplete).
;;
;;  - For regular usage, see the function `org-reftable'.
;;

;;; Change Log:

;;   [2012-12-07 Fr] Version 2.0.0:
;;    - renamed the package from \"org-refer-by-number\" to \"org-reftable\"
;;    - The format of the reference table has changed ! You need to bring
;;      your existing table into the new format by hand (which however is
;;      easy and explained below)
;;    - Reference table can be sorted after usage count or date of last access
;;    - Ask user explicitly, which command to invoke

;;   [2012-09-22 Sa] Version 1.5.0:
;;    - New command "sort" to sort a buffer or region by reference number
;;    - New commands "highlight" and "unhighlight" to mark references

;;   [2012-07-13 Fr] Version 1.4.0:
;;    - New command "head" to find a headline with a reference number

;;   [2012-04-28 Sa] Version 1.3.0:
;;    - New commands occur and multi-occur
;;    - All commands can now be invoked explicitly
;;    - New documentation
;;    - Many bugfixes

;;   [2011-12-10 Sa] Version 1.2.0:
;;    - Fixed a bug, which lead to a loss of newly created reference numbers
;;    - Introduced single and double prefix arguments
;;    - Started this Change Log

;;; Code:

(require 'org-table)
(require 'cl)

(defvar org-reftable-preferred-command nil
  "Preferred command when choosing")

(defvar org-reftable-commands '(occur head new enter leave goto help reorder sort update highlight unhighlight)
  "List of commands known to org-reftable:
 

  occur: If you supply a keyword (text): Apply emacs standard
    occur operation on the table of references; ask for a
    string (keyword) to select lines. Occur will only show you
    references which contain the given keyword, so you can easily
    find the right one

   If you supply a reference (number): Apply emacs standard
   multi-occur operation all org-mode buffers to search for a
   specific reference

  head: Scan all headings until the first one with the given
    reference number is found

  new: Create a new reference. Copy any previously selected text

  leave: Leave the table of references. If the last command has
    been \"new\", the new reference is copied and ready to yank

  enter: Just enter the node with the table of references

  goto: Search for a specific references within the table of
    references

  help: Show this list of commands

  all: Show all commands including the less frequently used ones
    given below.

  reorder: Temporarily reorder the table of references, e.g. by
    cound or last access

  sort: Sort a set of lines (either the active region or the
    whole buffer) by the references found within each line

  update: For the given reference update the line in the
    reference table

  highlight: Highlight references in region or buffer

  unhighlight: Remove highlights


When prompting for a command, org-reftable puts the most likely
chosen one (e.g. \"occur\" or \"new\") at the front of the list,
so that you may just type RET. If this command needs additional
input (like e.g. \"occur\" does, which needs a string to search
for), you may supply this input right away, although you are
still beeing prompted for the command (in that case your input
will not match any of the given choices).

")

(defvar org-reftable-commands-some '(occur head new leave enter goto all help)
  "Subset of org-reftable-commands shown initially" )

(defvar org-reftable-id nil 
  "Id of the Org-mode node, which contains the reference table.

Read below, on how to set up things. See the documentation of
`org-reftable' for normal usage after setup.

Setup requires two steps:

 - Adjust your .emacs initialization file

 - Create a suitable org-mode node


Here are the lines, you should add to your .emacs:

  (require 'org-reftable)
  ;; Later you should probably change this id, as will be explained below
  (setq org-reftable-id \"00e26bef-1929-4110-b8b4-7eb9c9ab1fd4\")
  ;; Optionally assign a key; pick your own favorite
  (global-set-key (kbd \"C-+\") 'org-reftable)

Do not forget to restart emacs to make these lines effective.

The id given above is an example, yours can be different.


As a second step you need to create the org-mode node, where your
reference numbers will be stored. It may look like this:


  * org-reftable
    :PROPERTIES:
    :ID:       00e26bef-1929-4110-b8b4-7eb9c9ab1fd4
    :END:


    |     | comment            |         |         |               |
    | ref | ;c                 | count;s | created | last-accessed |
    |-----+--------------------+---------+---------+---------------|
    | R1  | My first reference |         |         |               |



You may just want to copy this node into one of your org-files.
Many things however can or should be adjusted:

 - The node needs not be a top level node.

 - Its name is completely at you choice. The node is found
   through its ID.

 - There are two lines of headings above the first hline. The
   first one is ignored by org-reftable, and you can use them to
   give meaningful names to columns; the second line however
   contains configuration information for org-reftable; please
   read further below for its format.

 - The sequence of columns does not matter. You may reorder them
    any way you like; e.g. make the comment-column the last
    columns within the table.

 - You can add further columns or even remove the
   \"Comment\"-column. The columns \"ref\" and \"created\"
   however are required. Columns \"cound\" and \"last-accessed\"
   are optional, but highly suggested anyway.

 - Your references need not start at \"R1\"; However, having an
   initial row is required (it servers a template for subsequent
   references).

 - Your reference need not have the form \"R1\"; you may just as
   well choose any text, that contains a single number,
   e.g. \"reference-{1}\" or \"#1\" or \"++1++\" or \"-1-\". The
   function `org-reftable' will inspect your first reference and
   create all subsequent references in the same way.
    
 - You may want to change the ID-Property of the node above and
   create a new one, which is unique (and not just a copy of
   mine). You need to change it in the lines copied to your .emacs
   too. However, this is not strictly required to make things
   work, so you may do this later, after trying out this package.


Optionally you may tweak the second header line to adjust
`org-reftable' a bit. In the ecample above it looks like this:


    | ref | ;c                 | count;s | created | last-accessed |
    |-----+--------------------+---------+---------+---------------|

The different fields have different meanings:

 - ref : this denotes the column which contains you references

 - ;c : the flag \"c\" (\"c\" for \"copy\") denotes this column
   as the one beeing copied on command \"leave\". In the example above,
   it is the comment-column.

 - count;s : this is the column which counts last access, whence
   \"count\"; the flag \"s\" stands for \"sort\", so this is the
   column after which the table is sorted. You may also sort
   after columns \"ref\" or \"last-accessed\".

 - created : date when this reference was created.

 - last-accessed : date and time, when this reference was last accessed.


After this two-step setup process you may invoke `org-reftable'
to create a new reference number; read there for instructions on
normal usage.

If you have an existing reference table from a version of
org-reftable before 2.0 (in fact earlier versions were rather
named org-refer-by-number), you need to add a second headline
like this, just about the hline to reflect the usage of columns
in earlier versions:

  | ref | created |

This will mark the first column as the actual references and the
second column as the date of creation. However, to take advantage
of the new features you should also add two other columns \"count;s\" 
(marked as the sort-column) and \"last-accessed\".

")

(defvar org-reftable-windowconfig-before nil 
  "Saved window-configuration for `org-reftable'.
This variable is only used internally.")

(defvar org-reftable-marker-outside-before nil 
  "Saved position in reftable-buffer bit outside of reftable (if at all).
This variable is only used internally.")

(defvar org-reftable-last-action nil 
  "Last action performed by `org-reftable'.
This variable is only used internally.")

(defvar org-reftable-occur-buffer nil
  "Buffer (if any) with result from occur or multi-occur.
This variable is only used internally.")

(defvar org-reftable-ref-regex nil
  "Regular expression to search for
This variable is only used internally.")



(defun org-reftable  (&optional what search) 
  "Create, search and look up numbers from a dedicated reference table.
These numbers (e.g. \"R237\" or \"-455-\") may be used to refer to:

 - Nodes in Org-mode (insert them into the heading)

 - Things outside of org (e.g. mailfolders, directories, reports or
   pieces of paper)

The table is kept sorted for most frequently or most recently used
reference numbers. Additionally, lines can be selected by keywords, so
that specific references can be found easily.


Read below for a detailed description of this function. See the
documentation of `org-reftable-id' for the necessary
setup.


The function `org-reftable' operates on a dedicated table (called
the reference table) within a special Org-mode node. The node has
to be created as part of your initial setup. Each line of the
reference table contains:

 - A reference 

 - Its respective creation date

 - A number; counting, how often each reference has been
   used. This number is updated automatically and the table can
   be sorted according to it, so that most frequently used
   references appear at the top of the table and can be spotted
   easily.

 - Date and time of last access. This column can alternatively be
   used to sort the table.

The reference table is found through the id of the containing
node; this id must be stored within `org-reftable-id' (see there
for details).


The function `org-reftable' is the only interactive function of
this package and its sole entry point; it offers several commands
to create, find and look up these reference numbers. All of them
are described in the docstring of `org-reftable-commands' (see
there for details).


Finally, org-reftable can also be invoked from elisp; the two
optional arguments to be accepted are:

  search : string to search for
    what : symbol of the command to invoke

An example would be:

 (org-reftable \"237\" 'head)   ;; find heading with ref 237

"

  (interactive "P")

  (let (within-node        ; True, if we are within node with reference table
        result-is-visible  ; True, if node or occur is visible in any window
        ref-node-buffer-and-point  ; cons with buffer and point of reference node
        below-cursor       ; word below cursor
        active-region      ; active region (if any)
        guarded-search     ; with guard against additional digits
        commands           ; currently active set of selectable commands
        what-adjusted      ; True, if we had to adjust what
        what-input         ; Input on what question (need not necessary be "what")
        reorder-once       ; Column to use for single time sorting
        parts              ; Parts of a typical reference number (which
                           ; need not be a plain number); these are:
        head               ; Any header before number (e.g. "R")
        maxref             ; Maximum number from reference table (e.g. "153")
        tail               ; Tail after number (e.g. "}" or "")
        ref-regex          ; Regular expression to match a reference
        numcols            ; Number of columns in reference table
        columns            ; Associate column names with numbers
        kill-new-text      ; Text that will be appended to kill ring
        message-text       ; Text that will be issued as an explanation,
                           ; what we have done
        )

    ;;
    ;; Examine current buffer and location, before turning to reference table
    ;;

    ;; Get the content of the active region or the word under cursor
    (if (and transient-mark-mode
             mark-active)
        (setq active-region (buffer-substring (region-beginning) (region-end))))
    (setq below-cursor (thing-at-point 'symbol))


    ;; Find out, if we are within reference table or not
    (setq within-node (string= (org-id-get) org-reftable-id))

    ;; Find out, if point in any window is within node with reference table
    (mapc (lambda (x) (with-current-buffer (window-buffer x)
                        (when (or 
                               (string= (org-id-get) org-reftable-id)
                               (eq (window-buffer x) 
                                   org-reftable-occur-buffer))
                          (setq result-is-visible t))))
          (window-list))
        


    ;;
    ;; Get decoration of references and highest number from reference table
    ;;

    ;; Find node
    (setq ref-node-buffer-and-point (org-reftable-id-find))
    (unless ref-node-buffer-and-point
      (org-reftable-report-setup-error 
       (format "Cannot find node with id \"%s\"" org-reftable-id)))

    ;; Get configuration of reftable
    (with-current-buffer (car ref-node-buffer-and-point)
      (unless (string= (org-id-get) org-reftable-id)
        ;; Get marker for point within reftable-buffer, but only if outside
        ;; of reftable (if point is within reftable, we will try to stay at
        ;; the same ref)
        (setq org-reftable-marker-outside-before (point-marker))
        (goto-char (cdr ref-node-buffer-and-point)))

      ;; parse table
      (setq parts (org-reftable-parse-and-adjust-table reorder-once)))
    
    ;; Give names to parts of configuration
    (setq head (nth 0 parts))
    (setq maxref (nth 1 parts))
    (setq tail (nth 2 parts))
    (setq numcols (nth 3 parts))
    (setq columns (nth 4 parts))
    (setq ref-regex (nth 5 parts))
    
    ;;
    ;; Find out, what we are supposed to do
    ;;

    (if (equal what '(4)) (setq what 'leave))

    ;; Set preferred action, that will be the default choice
    (setq org-reftable-preferred-command
          (if within-node
              (if (eq org-reftable-last-action 'new)
                  'leave
                'occur)
          (if active-region
              'new
           (if (and below-cursor (string-match ref-regex below-cursor))
            'occur
            nil))))
    
    ;; Ask user
    (unless what
      (setq commands (copy-list org-reftable-commands-some))
      (while (progn
               (setq what-input
                     (org-icompleting-read 
                      "Please choose: " 
                      (mapcar 'symbol-name 
                              ;; Construct unique list of commands with
                              ;; preferred one at front
                              (delq nil (delete-dups 
                                         (append 
                                          (list org-reftable-preferred-command)
                                          commands))))
                      nil nil))
               (setq what (intern what-input))
               
               ;; user is not required to input one of the commands; if
               ;; not, take the first one and use the original input for
               ;; next question
               (if (memq what commands)
                   ;; input matched one element of list, dont need original
                   ;; input any more
                   (setq what-input nil)
                 ;; what-input will be used for next question, use first
                 ;; command for what
                 (setq what (or org-reftable-preferred-command
                                (first commands)))
                 ;; remove any trailing dot, that user might have added to
                 ;; disambiguate his input
                 (if (equal (substring what-input -1) ".")
                     ;; but do this only, if dot was really necessary to
                     ;; disambiguate
                     (let ((shortened-what-input (substring what-input 0 -1)))
                       (unless (test-completion shortened-what-input 
                                            (mapcar 'symbol-name 
                                                    org-reftable-commands))
                           (setq what-input shortened-what-input)))))
                     

               ;; ask for reorder in loop, because we have to ask for
               ;; what right again
               (if (eq what 'reorder)
                   (setq reorder-once
                         (intern
                          (org-icompleting-read 
                           "Please choose column to reorder reftable once: " 
                           (mapcar 'symbol-name '(ref count last-accessed))
                           nil t))))
               
               ;; offer extended selection of commands, if asked for
               (if (eq what 'all)
                   (setq commands (copy-list org-reftable-commands)))

               ;; maybe ask initial question again
               (memq what '(reorder all)))))


    ;;
    ;; Get search, if required
    ;;

    ;; These actions need a search string:
    (when (memq what '(goto occur head update))

      ;; Maybe we've got a search string from the arguments
      (unless search
        (let (search-from-table
              search-from-cursor)
          
          ;; Search string can come from several sources:
          ;; From ref column of table
          (when within-node
            (save-excursion (setq search-from-table (org-table-get-field (cdr (assoc 'ref columns)))))
            (if (string= search-from-table "") (setq search-from-table nil)))      
          ;; From string below cursor
          (when (and (not within-node)
                     below-cursor
                     (string-match (concat "\\(" ref-regex "\\)") 
                                   below-cursor))
            (setq search-from-cursor (match-string 1 below-cursor)))
          
          ;; Depending on requested action, get search from one of the sources above
          (cond ((eq what 'goto)
                 (setq search (or what-input search-from-cursor)))
                ((memq what '(head occur))
                 (setq search (or what-input search-from-table search-from-cursor))))))


      ;; If we still do not have a search string, ask user explicitly
      (unless search
        
        (if what-input 
            (setq search what-input)
          (setq search (read-from-minibuffer
                        (cond ((memq what '(goto occur head))
                               "Text or reference number to search for: ")
                              ((eq what 'update)
                               "Reference number to update: ")))))

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


      ;;
      ;; Do some sanity checking before really starting
      ;;

      ;; Correct requested action, if nothing to search
      (when (and (not search)
                 (memq what '(search occur head)))
        (setq what 'enter)
        (setq what-adjusted t))

      ;; Check for invalid combinations of arguments; try to be helpful
      (if (string-match ref-regex search)
          (progn
            ;; Count searches and update last access date
            (if search (org-reftable-update-reference-line search columns))
            (if (eq what 'occur) (setq what 'multi-occur)))
        (when (memq what '(goto head))
            (error "Can do '%s' only for a number (not '%s'), try 'occur' to search for text" what search))))

    
    ;;
    ;; Prepare
    ;;

    ;; Move into table, if outside
    (when (memq what '(enter new goto occur multi-occur))
      ;; Save current window configuration
      (when (or (not result-is-visible)
                (not org-reftable-windowconfig-before))
        (setq org-reftable-windowconfig-before (current-window-configuration)))

      ;; Switch to reference table
      (org-pop-to-buffer-same-window (car ref-node-buffer-and-point))
      (goto-char (cdr ref-node-buffer-and-point))
      (show-subtree)
      (org-show-context))


    ;;
    ;; Actually do, what is requested
    ;;

    (cond


     ((eq what 'help)
      
      (let ((help-what
             ;; which sort of help ?
             (intern
              (concat 
               "help-"
               (org-icompleting-read 
                "Help on: "
                (mapcar 'symbol-name '(commands usage setup)) 
                nil t)))))

        ;; help is taken from docstring of functions or variables
        (cond ((eq help-what 'help-commands)
               (org-reftable-show-help 'org-reftable-commands))
              ((eq help-what 'help-usage)
               (org-reftable-show-help 'org-reftable))
              ((eq help-what 'help-setup)
               (org-reftable-show-help 'org-reftable-id)))))


     ((eq what 'multi-occur) 
      
      ;; Conveniently position cursor on number to search for
      (org-reftable-goto-top)
      (let (found (initial (point)))
        (while (and (not found)
                    (forward-line)
                    (org-at-table-p))
          (save-excursion 
            (setq found (string= search 
                                 (org-trim (org-table-get-field (cdr (assoc 'ref columns))))))))
        (if found 
            (org-table-goto-column (cdr (assoc 'ref columns)))
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
              (setq org-reftable-occur-buffer (get-buffer "*Occur*"))
              (other-window 1)
              (toggle-truncate-lines 1))
          (setq message-text (format "Did not find '%s'" search)))))


     ((eq what 'head)

      (message (format "Scanning headlines for '%s' ..." search))
      (let (buffer point)
        (if (catch 'found
              (progn
                ;; loop over all headlines, stop on first match
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

        ;; If we are within the occur-buffer, switch over to get current line
        (if (and (string= (buffer-name) "*Occur*")
                 (eq org-reftable-last-action 'occur))
            (occur-mode-goto-occurrence))

        (let (copy-column)              
          ;; Try to copy requested column
          (setq copy-column (cdr (assoc 
                                  (if (eq org-reftable-last-action 'new)
                                      'goto
                                    'copy)
                                  columns)))
            
          ;; Add to kill ring
          (if (memq org-reftable-last-action '(new enter goto occur))
              (setq kill-new-text 
                    (org-trim (org-table-get-field copy-column))))))

      ;; Restore position within buffer with reference table
      (with-current-buffer (car ref-node-buffer-and-point)
        (when org-reftable-marker-outside-before
          (goto-char (marker-position org-reftable-marker-outside-before))
          (move-marker org-reftable-marker-outside-before nil)))

      ;; Restore windowconfig
      (if org-reftable-windowconfig-before 
          (progn  
            ;; Restore initial window configuration
            (set-window-configuration org-reftable-windowconfig-before)
            (setq org-reftable-windowconfig-before nil)
            ;; Goto initial position
            (recenter)
            (setq message-text "Back"))
        ;; We did not have a window-configuration to restore, so we cannot
        ;; pretend we have returned back
        (setq message-text "Cannot leave; nowhere to go to")
        (setq kill-new-text nil)))


     ((eq what 'goto)

      ;; Go downward in table to requested reference
      (let (found (initial (point)))
        (org-reftable-goto-top)
        (while (and (not found)
                    (forward-line)
                    (org-at-table-p))
          (save-excursion 
            (setq found 
                  (string= search 
                           (org-trim (org-table-get-field (cdr (assoc 'ref columns))))))))
        (if found
            (progn
              (setq message-text (format "Found '%s'" search))
              (org-table-goto-column (cdr (assoc 'ref columns)))
              (if (looking-back " ") (backward-char)))
          (setq message-text (format "Did not find '%s'" search))
          (goto-char initial)
          (forward-line)
          (setq what 'missed))))


     ((eq what 'occur)

      ;; search for string: occur
      (save-restriction
        (org-narrow-to-subtree)
        (occur search)
        (widen)
        (if (get-buffer "*Occur*")
            (with-current-buffer "*Occur*"

              ;; install helpful keyboard-shortcuts within occur-buffer
              (let ((keymap (make-sparse-keymap)))
                (set-keymap-parent keymap occur-mode-map)

                (define-key keymap (kbd "RET") 
                  (lambda () (interactive) 
                    (org-reftable-occur-helper 'head)))

                (define-key keymap (kbd "<C-return>") 
                  (lambda () (interactive) 
                    (org-reftable-occur-helper 'multi-occur)))

                (use-local-map keymap))
              (setq org-reftable-ref-regex ref-regex)

              ;; insert some help text
              (other-window 1)
              (toggle-truncate-lines 1)
              (let ((inhibit-read-only t)) 
                (insert (substitute-command-keys 
                         "Type RET to find heading, C-RET for multi-occur, \\[next-error-follow-minor-mode] for follow-mode.\n\n")))
              (forward-line 1)
              (setq message-text
                    (format  "Occur for '%s'" search)))
          (setq message-text
                (format "Did not find any matches for '%s'" search)))))


     ((eq what 'new)

      ;; add a new row
      (org-reftable-goto-top)
      (let ((new (format "%s%d%s" head (1+ maxref) tail)))

        (org-table-insert-row)

        ;; fill special columns with standard values
        (org-table-goto-column (cdr (assoc 'ref columns)))
        (insert new)
        (org-table-goto-column (cdr (assoc 'created columns)))
        (org-insert-time-stamp nil nil t)

        ;; goto first nonempty field
        (catch 'empty
          (dotimes (col numcols)
            (org-table-goto-column (+ col 1))
            (if (string= (org-trim (org-table-get-field)) "")
                (throw 'empty t)))
          ;; none found, goto first
          (org-table-goto-column 1))

        (org-table-align)
        (if active-region (setq kill-new-text active-region))
        (setq message-text (format "Adding a new row '%s'" new))))


     ((eq what 'enter)

      ;; simply go into table
      (org-reftable-goto-top)
      (show-subtree)
      (recenter)
      (if what-adjusted
          (setq message-text "Nothing to search for; at reference table")
        (setq message-text "At reference table")))


     ((eq what 'sort)

      ;; sort lines according to contained reference
      (let (begin end where)
        (catch 'aborted
          ;; either active region or whole buffer
          (if (and transient-mark-mode
                   mark-active)
              ;; sort only region
              (progn
                (setq begin (region-beginning))
                (setq end (region-end))
                (setq where "region"))
            ;; sort whole buffer
            (setq begin (point-min))
            (setq end (point-max))
            (setq where "whole buffer")
            ;; make sure
            (unless (y-or-n-p "Sort whole buffer ")
              (setq message-text "Sort aborted")
              (throw 'aborted nil)))
          
          (save-excursion
            (save-restriction
              (goto-char (point-min))
              (narrow-to-region begin end)
              (sort-subr nil 'forward-line 'end-of-line 
                         (lambda ()
                           (if (looking-at (concat "^.*\\b" ref-regex "\\b"))
                               (string-to-number (match-string 1))
                             0))))
            (highlight-regexp ref-regex)
            (setq message-text (format "Sorted %s from character %d to %d, %d lines" 
                                       where begin end
                                       (count-lines begin end)))))))
    

     ((eq what 'update)

      ;; simply update line in reftable
      (save-excursion
        (beginning-of-line)
        (if (org-reftable-update-reference-line search columns)
            (setq message-text (format "Updated reference '%s'" search))
          (setq message-text (format "Did not find reference '%s'" search)))))


     ((memq what '(highlight unhighlight))

      (let ((where "buffer"))
        (save-excursion
          (save-restriction
            (when (and transient-mark-mode
                       mark-active)
              (narrow-to-region (region-beginning) (region-end))
              (setq where "region"))

            (if (eq what 'highlight)
                (progn
                  (highlight-regexp ref-regex)
                  (setq message-text (format "Highlighted references in %s" where)))
              (unhighlight-regexp ref-regex)
              (setq message-text (format "Removed highlights for references in %s" where)))))))


     (t (error "This is a bug: Unmatched condition '%s'" what)))


    ;; remember what we have done for next time
    (setq org-reftable-last-action what)
    
    ;; tell, what we have done and what can be yanked
    (if kill-new-text (setq kill-new-text 
                            (substring-no-properties kill-new-text)))
    (if (string= kill-new-text "") (setq kill-new-text nil))
    (let ((m (concat 
              message-text
              (if (and message-text kill-new-text) 
                  " and r" 
                (if kill-new-text "R" ""))
              (if kill-new-text (format "eady to yank '%s'" kill-new-text) ""))))
      (unless (string= m "") (message m)))
    (if kill-new-text (kill-new kill-new-text))))



(defun org-reftable-parse-and-adjust-table (&optional sort-column)
  "Trim reference table, only used internally"

  (let ((maxref 0)
        top
        bottom
        field
        parts
        numcols
        columns
        head
        tail
        ref-regex
        initial-ref
        initial-point)

    (setq initial-point (point))
    (org-reftable-goto-top)
    (setq top (point))
    
    (goto-char top)
    
    ;; count columns
    (org-table-goto-column 100)
    (setq numcols (- (org-table-current-column) 1))
    (org-table-goto-column 1)
    
    ;; get contents of columns
    (forward-line -2)
    (unless (org-at-table-p)
      (org-reftable-report-setup-error 
       "Reference table starts with a hline" t))
    
    (setq columns (org-reftable-parse-headings numcols))
    
    ;; Go beyond end of table
    (while (org-at-table-p) (forward-line 1))
    
    ;; Kill all empty rows at bottom
    (while (progn
             (forward-line -1)
             (org-table-goto-column 1)
             (string= "" (org-trim (org-table-get-field (cdr (assoc 'ref columns))))))
      (org-table-kill-row))
    (forward-line)
    (setq bottom (point))
    (forward-line -1)
    
    ;; Retrieve any decorations around the number within ref-field of
    ;; the first row
    (goto-char top)
    (setq field (org-trim (org-table-get-field  (cdr (assoc 'ref columns)))))
    (or (numberp (string-match "^\\([^0-9]*\\)\\([0-9]+\\)\\([^0-9]*\\)$" field))
        (org-reftable-report-setup-error 
         (format "reference column in first line of reference table '%s' does not contain a number" field) t))
    
    ;; These are the decorations used within the first row of the
    ;; reference table
    (setq head (match-string 1 field))
    (setq tail (match-string 3 field))
    (setq ref-regex (concat (regexp-quote head)
                            "\\([0-9]+\\)" 
                            (regexp-quote tail)))

    ;; Save initial ref
    (save-excursion
      (let (field)
        (goto-char initial-point)
        (setq field (org-table-get-field (cdr (assoc 'ref columns))))
        (if (string-match ref-regex field)
            (setq initial-ref (concat head (match-string 1 field) tail)))))

    ;; Go through table to find maximum number
    (let ((ref 0)
          field)
      (while (org-at-table-p) 
        (setq field (org-trim (org-table-get-field (cdr (assoc 'ref columns)))))
        (if (string-match ref-regex field)
            (setq ref (string-to-number (match-string 1 field)))
          
          (unless (string= "" field)
            (org-reftable-report-setup-error 
             (format "Reference field in line of reference table '%s' does not contain a number" field) t)))
        (if (> ref maxref) (setq maxref ref))
        (forward-line 1)))
    
    (setq parts (list head maxref tail numcols columns ref-regex))
    
    ;; sort table after sort-column
    (unless sort-column (setq sort-column (cdr (assoc 'sort columns))))
    (goto-char top)
    (forward-line 0)
    (save-restriction
      (narrow-to-region (point) bottom)
      (sort-subr t
                 'forward-line 
                 'end-of-line 
                 (lambda ()
                   (let (ref
                         (ref-field (org-table-get-field 
                                     (cdr (assoc 'ref columns)))))
                     (string-match ref-regex ref-field)
                     ;; get reference with leading zeroes, so it can be
                     ;; sorted as text
                     (setq ref (format 
                                "%06d" 
                                (string-to-number 
                                 (match-string 1 ref-field))))
                     
                     ;; Construct different sort-keys according to
                     ;; requested sort column; append ref as a secondary
                     ;; sort key

                     ;; 
                     (cond ((eq sort-column 'count)
                            (concat (format 
                                     "%08d" 
                                     (string-to-number 
                                      (org-table-get-field 
                                       (cdr (assoc 'count columns))))) 
                                    " " ref))
                           
                           ((eq sort-column 'last-accessed)
                            (concat (org-table-get-field 
                                     (cdr (assoc 'last-accessed columns))) 
                                    " " ref))
                           
                           ((eq sort-column 'ref)
                            ref)
                           
                           (t 
                            (error "Bug !")))))
                 nil
                 'string<)
      )

    ;; align table
    (org-table-align)
    
    ;; go back to top of table
    (goto-char top)

    ;; Goto back to initial ref, because reformatting of table above might
    ;; have moved point
    (when initial-ref
      (while (and (org-at-table-p)
                  (not (string= initial-ref (org-trim (org-table-get-field (cdr (assoc 'ref columns)))))))
        (forward-line))
      ;; did not find ref, go back to top
      (if (not (org-at-table-p)) (goto-char top)))

    parts))



(defun org-reftable-goto-top ()
  "Goto topmost reference line in reftable"

  ;; go to heading of node
  (while (not (org-at-heading-p)) (forward-line -1))
  (forward-line 1)
  ;; go to table within node, but make sure we do not get into another node
  (while (and (not (org-at-heading-p))
              (not (org-at-table-p))
              (not (eq (point) (point-max)))) 
    (forward-line 1))
     
  ;; check, if there really is a table
  (unless (org-at-table-p)
    (org-reftable-report-setup-error 
     "Cannot find reference table within reference node" t))

  ;; go to first hline
  (while (and (not (org-at-table-hline-p))
              (org-at-table-p))
    (forward-line 1))
     
  ;; and check
  (unless (org-at-table-hline-p)
    (org-reftable-report-setup-error 
     "Cannot find hline within reference table" t))      

  (forward-line 1)
  (org-table-goto-column 1))



(defun org-reftable-id-find ()
  "Find org-reftable-id"
  (let ((marker (org-id-find org-reftable-id 'marker))
        marker-and-buffer)

    (if marker 
        (progn 
          (setq marker-and-buffer (cons (marker-buffer marker) (marker-position marker)))
          (move-marker marker nil)
          marker-and-buffer)
      nil)))



(defun org-reftable-parse-headings (numcols)
  "Parse headings to find special columns"

  (let (columns)

    ;; Associate names of special columns with column-numbers
    (setq columns (copy-tree '((ref . 0) (created . 0) (last-accessed . 0) 
                               (count . 0) (sort . nil) (copy . nil))))

    ;; For each column
    (dotimes (col numcols)
      (let* (field-flags ;; raw heading, consisting of file name and maybe
                         ;; flags (seperated by ";")
             field       ;; field name only
             field-symbol ;; and as a symbol
             flags       ;; flags from field-flags
             found)

        ;; parse field-flags into field and flags
        (setq field-flags (org-trim (org-table-get-field (+ col 1))))
        (if (string-match "^\\([^;]*\\);\\([a-z]+\\)$" field-flags)
            (progn 
              (setq field (downcase (or (match-string 1 field-flags) "")))
              ;; get flags as list of characters
              (setq flags (mapcar 'string-to-char 
                                  (split-string 
                                   (downcase (match-string 2 field-flags)) 
                                   "" t))))
          ;; no flags
          (setq field field-flags))

        (unless (string= field "") (setq field-symbol (intern (downcase field))))

        ;; Check, that no flags appear twice
        (mapc (lambda (x)
                (when (memq (car x) flags)
                  (if (cdr (assoc (cdr x) columns))
                      (org-reftable-report-setup-error 
                       (format "More than one heading is marked with flag '%c'" (car x)) t))))
              '((?s . sort)
                (?c . copy)))
        
        ;; Process flags
        (if (memq ?s flags)
            (setcdr (assoc 'sort columns) field-symbol))
        (if (memq ?c flags)
            (setcdr (assoc 'copy columns) (+ col 1)))
        
        ;; Store columns in alist
        (setq found (assoc field-symbol columns))
        (when found
          (if (> (cdr found) 0) 
              (org-reftable-report-setup-error 
               (format "'%s' appears two times as column heading" (downcase field)) t))
          (setcdr found (+ col 1)))))

    ;; check if all necessary informations have been specified
    (unless (> (cdr (assoc 'ref columns)) 0)
      (org-reftable-report-setup-error 
       "column 'ref' has not been set" t))

    ;; use ref as a default sort-column
    (unless (cdr (assoc 'sort columns))
      (setcdr (assoc 'sort columns) 'ref))
    columns))



(defun org-reftable-report-setup-error (text &optional switch-to-node)
  "Report error, which might be related with incomplete setup; offer help"

  (when switch-to-node 
    (org-id-goto org-reftable-id)
    (delete-other-windows))
  
  (when (y-or-n-p (concat
                   text 
                   ";\n"
                   "the correct setup is explained in the documentation of 'org-reftable-id'.\n" 
                   "Do you want to read it ? "))
    (org-reftable-show-help 'org-reftable-id))

  (error "")
  (setq org-reftable-windowconfig-before nil)
  (move-marker org-reftable-marker-outside-before nil)
  (setq org-reftable-last-action 'leave))



(defun org-reftable-show-help (function-or-variable)
  "Show help on command or function and trim help buffer displayed"

  (let ((isfun (functionp function-or-variable)))
    ;; bring up help-buffer for function or variable
    (if isfun
        (describe-function function-or-variable)
      (describe-variable function-or-variable))

    
    ;; clean up help-buffer
    (pop-to-buffer "*Help*")
    (let ((inhibit-read-only t)) 
      (goto-char (point-min))
      (while (progn
               (kill-line 1)
               (not (looking-at 
                     (if isfun
                         "(" 
                       "Documentation:")))))
      (kill-line (if isfun 2 1))
      (goto-char (point-max))
      (kill-line -2)
      (goto-char (point-min)))))
                 


(defun org-reftable-update-reference-line (reference columns)
  "Update access count and time of reference number"

  (let ((initial (point))
        found
        (ref-node-buffer-and-point (org-reftable-id-find)))
    (with-current-buffer (car ref-node-buffer-and-point)
      (goto-char (cdr ref-node-buffer-and-point))
      (org-reftable-goto-top)
      (while (and (org-at-table-p)
                  (if (string= reference (org-trim (org-table-get-field (cdr (assoc 'ref columns)))))
                      (progn (org-table-get-field (cdr (assoc 'count columns))
                                                  (number-to-string 
                                                   (+ 1 (string-to-number 
                                                         (org-table-get-field (cdr (assoc 'count columns)))))))
                             (org-table-goto-column (cdr (assoc 'last-accessed columns)))
                             (org-table-blank-field)
                             (org-insert-time-stamp nil t t)
                             (org-table-align)
                             (setq found t)
                             nil)
                    t))
        (forward-line))
      (goto-char initial))
    found))



(defun org-reftable-occur-helper (action)
  "Internal helper function for occur in org-reftable"
  (save-excursion
    (beginning-of-line)
    (if (looking-at (concat ".*\\b\\(" org-reftable-ref-regex "\\)\\b"))
        (org-reftable action (match-string 1)))))


(provide 'org-reftable)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; org-reftable.el ends here

