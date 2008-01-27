;;; org-link-irc.el --- Store links to IRC sessions.
;; Copyright (C) 2008 Phil Jackson
;;
;; Author: Philip Jackson <emacs@shellarchive.co.uk>
;; Keywords: erc, irc, link, org
;; Version: 0.01
;;
;; This file is not yet part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; TODO: Make the following betterer...er.
;;
;; Setting `org-link-irc-link-to-logs' to non-nil will cause
;; `org-store-link' to link to a the log of the current IRC session,
;; otherwise an IRC style link in the form of:
;;
;; irc:/irc.freenode.net/#emacs/bob
;; 
;;; Code:

(require 'org)
(require 'cl)

(defvar org-link-irc-client 'erc)
(defvar org-link-irc-link-to-logs t
  "non-nil will store a link to the logs, nil will store an irc:/ link")

;; Generic functions/config (extend for other clients)

(add-to-list 'org-store-link-functions
             'org-link-irc-store-link)

(org-add-link-type "irc" 'org-link-irc-visit nil)

(defun org-link-irc-visit (link)
  "Dispatch to the correct visit function based on the client"
  (let ((link (org-link-irc-parse-link link)))
    (cond
      ((eq org-link-irc-client 'erc)
       (org-link-irc-visit-erc link))
      (t
       (error "erc only known client")))))

(defun org-link-irc-parse-link (link)
  "Get a of irc link attributes where `link' looks like
server:port/chan/user (port, chan and user being optional)."
  (let* ((parts (split-string link "/" t))
         (len (length parts)))
    (when (or (< len 1) (> len 3))
      (error "Failed to parse link needed 1-3 parts, got %d." len))
    (setcar parts (split-string (car parts) ":" t))
    parts))

;;;###autoload
(defun org-link-irc-store-link ()
  "Dispatch to the appropreate function to store a link to
something IRC related"
  (cond
   ((eq major-mode 'erc-mode)
    (org-link-irc-erc-store-link))))

;; ERC specific functions

(defun org-link-irc-erc-store-link ()
  "Return an org compatible file:/ link which links to a log file
of the current ERC session"
  (if org-link-irc-link-to-logs
      (let ((erc-line (buffer-substring-no-properties
                       (point-at-bol) (point-at-eol))))
        (if (erc-logging-enabled nil)
            (progn
              (erc-save-buffer-in-logs)
              (with-current-buffer (find-file-noselect (erc-current-logfile))
                (goto-char (point-max))
                (unless (search-backward erc-line nil t)
                  (error "Unable to find this line in the logs: '%s'"
                         erc-line))
                ;; a simple file link does here (use regexp?)
                (setq cpltxt (concat "file:" (abbreviate-file-name
                                              buffer-file-name)
				     (unless (equal erc-line (erc-prompt))
				       (concat "::" erc-line))))
                (org-make-link cpltxt)
                t))
	  (error "This ERC session is not being logged")))
    (let ((link (org-link-irc-get-erc-link)))
      (if link
	  (progn
	    (setq cpltxt (concat "irc:/" link))
	    (org-make-link cpltxt)
	    (setq link (org-link-irc-parse-link link))
	    (org-store-link-props :type "irc"         ; for remember
				  :server (car (car link))
				  :port (or (cadr (pop link))
					    erc-default-port)
				  :nick (pop link))
	    t)
	(error "Failed to create (non-log) ERC link")))))

(defun org-link-irc-get-erc-link ()
  "Return an org compatible irc:/ link from an ERC buffer"
  (let ((link (concat erc-server-announced-name ":"
                      erc-session-port)))
    (concat link "/"
            (if (and (erc-default-target)
		     (erc-channel-p (erc-default-target))
		     (get-text-property (point) 'erc-parsed)
		     (elt (get-text-property (point) 'erc-parsed) 1))
		;; we can get a nick
		(let ((nick
		       (car
			(erc-parse-user
			 (elt (get-text-property (point)
						 'erc-parsed) 1)))))
		  (concat (erc-default-target) "/"
			  (substring nick 1)))
	      (erc-default-target)))))

(defun org-link-irc-visit-erc (link)
  "Visit an ERC buffer based on criteria from the followed link"
  (let* ((server (car (car link)))
         (port (or (cadr (pop link)) erc-default-port))
         (server-buffer)
         (buffer-list
          (erc-buffer-filter
           (lambda nil
             (let ((tmp-server-buf (erc-server-buffer)))
	       (and tmp-server-buf
		    (with-current-buffer tmp-server-buf
		      (and
		       (string= erc-session-port port)
		       (string= erc-server-announced-name server)
		       (setq server-buffer tmp-server-buf)))))))))
    (if buffer-list
        (let ((chan-name (pop link)))
          ;; if we got a channel name then switch to it or join it
          (if chan-name
              (let ((chan-buf (find-if
                               (lambda (x)
                                 (string= (buffer-name x) chan-name))
                               buffer-list)))
                (if chan-buf
                    (progn
                      (switch-to-buffer chan-buf)
                      ;; if we got a nick, and they're in the chan,
                      ;; then start a chat with them
                      (let ((nick (pop link)))
                        (when nick
                          (if (find nick (erc-get-server-nickname-list)
                                    :test 'string=)
                              (progn
                                (goto-char (point-max))
                                (insert (concat nick ": ")))
			    (error "%s not found in %s" nick chan)))))
		  (progn
		    (switch-to-buffer server-buffer)
		    (erc-cmd-JOIN chan-name))))
	    (switch-to-buffer server-buffer)))
      ;; no server match, make new connection
      (erc-select :server server :port port))))

(provide 'org-link-irc)

;;; org-link-irc.el ends here
