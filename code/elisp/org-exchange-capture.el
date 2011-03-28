;; org-exchange-capture.el, v0.0.1
;; written by: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; License: GPLv3+

(require 'org-capture)

;; turn the e-mails sent MS-Exchange about invitation/appointments into org-TODOs
;; using 'org-capture'.

;; The idea is that you select (mark) the parts of the email you want to add to
;; your org-todo item, and then invoke M-x org-exchange-capture-invitation

;; Some caveats:

;;  - obviously, this is just a one-way copy, it won't make you 'accept' an
;;    invitation, nor does it get any updates

;;  - it seems to work with the emails I get from Exchange (I've encountered two
;;    different kinds). But there may be more; at least the ones I get that are
;;    in English, there are other versions as well. I'd be interested in
;;    extending the regexps with more cases.
;;
;;  - it does not take time-zones into account (yet)
;;
;;  NOTE:
;;  It does NOT handle yet:
;;     When: 12.08.2010 12:00-14:00 (GMT+02:00) Helsinki, ...
;;    When: \\([0-9]\\{2\\}\.[0-9]\\{2\\}\.[0-9]\\{4\\}\\)

;;    When: 20. syyskuuta 2010 12:00-12:45 (GMT+02:00) Helsinki,  ....

;;  When: Occurs every Thursday effective 19.08.2010 from 14:00 to 15:30
;;  (GMT+02:00) Helsinki, ...

;;  - it requires org-capture, which is fairly new; it should be easy to support
;;    org-remember as well though. Also, I only tested with Wanderlust as e-mail
;;    client; it *should* work with others as well though... 

;; Note that that the message buffer must be the active buffer;
;; ie. it won't work in the 'Summary' (Wanderlust)

(defun djcb-exchange-invite-time-to-org-date()
  "try to to find the Time/Date from an Exchange-invitation
e-mail in the current buffer, and convert it into an org-mode
date, or `nil' if it's not found."
  "get the time/date of an Outlook invite in org-mode notation"
  (let ((date) (time-begin) (time-end))
    (save-excursion
      (save-match-data
	(beginning-of-buffer)
	(if (re-search-forward
	      (concat "^When: \\([0-9]+ [a-z]+,? [0-9]\\{4\\}\\) "
		"\\([0-9]+:[0-9]+\\)-\\([0-9]+:[0-9]+\\)") nil t 1)
	  (progn
	    (setq
	      date (parse-time-string (match-string-no-properties 1))
	      time-begin (match-string-no-properties 2)
	      time-end (match-string-no-properties 3))
	    (format "<%d-%02d-%02d %s--%s>"
	      (elt date 5) (elt date 4) (elt date 3)
	    time-begin time-end))
	  (message "No match")
	  nil)))))

(defun djcb-exchange-invite-subject()
  "get the subject of an MS-Exchange invite e-mail in the current
buffer"
  (save-excursion
    (save-match-data
      (beginning-of-buffer)
      (when (re-search-forward "^Subject: \\(.*\\)" nil t 1)
	(match-string-no-properties 1)))))

(defun org-exchange-capture-invitation ()
  "capture the MS-Exchange invite e-mail in buffer into an
org-mode agenda item using the org-capture system. For this to
work, you'll need to add to your `org-capture-templates' an item
with a shortcut key of 'E', e.g.

     (\"E\" \"ExchangeInvite\" entry
       (file+headline \"todo.org\" \"Meetings\")
       \"* TODO %c\\n\")

any text you select (mark) in the buffer will be added to to
captured TODO; thus you can add the relevant details to the org TODO item.
"
  (interactive)
  (let(	(time (djcb-exchange-invite-time-to-org-date))
	(title (djcb-exchange-invite-subject))
	(txt
	   (if (use-region-p) (buffer-substring-no-properties (region-beginning)
				(region-end)) "")))
    (when time
      (kill-new (concat title " " time "\n\t" txt)) ;; hack: prepend to kill ring
      (org-capture nil "E"))))

(provide 'org-exchange-capture)
