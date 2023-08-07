#!/usr/bin/env sh
":"; # -*- mode: emacs-lisp; lexical-binding: t; -*-
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(defvar worg-publish-stop-on-error (member "--debug" command-line-args)
  "When non-nil, stop publishing process when an error is encountered.
This variable can be set when running publish.sh script:
  ./publish.sh --debug")

(add-to-list 'load-path "~/org-mode/lisp/")
(require 'ox-html)
(require 'cl-seq)
(load "/usr/share/emacs/site-lisp/elpa-src/htmlize-1.56/htmlize.el")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/elpa-src/ess-18.10.3snapshot/")

(setq make-backup-files nil
      debug-on-error t)

(setq org-confirm-babel-evaluate nil
      ess-ask-for-ess-directory nil
      ess-startup-directory nil
      org-html-style-default ""
      org-html-scripts ""
      org-html-htmlize-output-type 'css
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-validation-link nil
      org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
      org-ditaa-jar-path "/usr/bin/ditaa"
      org-html-preamble
      (with-temp-buffer (insert-file-contents "preamble.html") (buffer-string))
      org-html-postamble
      "<div id=\"license\"><p>Documentation from the orgmode.org/worg/ website (either in its HTML format or in its Org format) is licensed under the <a href=\"http://www.gnu.org/copyleft/fdl.html\">GNU Free Documentation License version 1.3</a> or later.  The code examples and css stylesheets are licensed under the <a href=\"http://www.gnu.org/licenses/gpl.html\">GNU General Public License v3</a> or later.</p></div>"
      org-html-head
      "<link rel=\"stylesheet\" title=\"Standard\" href=\"/worg/style/worg.css\" type=\"text/css\" />
<link rel=\"alternate stylesheet\" title=\"Zenburn\" href=\"/worg/style/worg-zenburn.css\" type=\"text/css\" />
<link rel=\"alternate stylesheet\" title=\"Classic\" href=\"/worg/style/worg-classic.css\" type=\"text/css\" />
<link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\" />")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (dot . t)
   (clojure . t)
   (org . t)
   (ditaa . t)
   (org . t)
   (scheme . t)
   (plantuml . t)
   (R . t)
   (gnuplot . t)))

(dolist (org-file (cl-remove-if
		   (lambda (n) (string-match-p "worg/archive/" n))
		   (directory-files-recursively default-directory "\\.org$")))
  (let ((html-file (concat (file-name-directory org-file)
			   (file-name-base org-file) ".html")))
    (if (and (file-exists-p html-file)
	     (file-newer-than-file-p html-file org-file))
	(message " [skipping] unchanged %s" org-file)
      (message "[exporting] %s" (file-relative-name org-file default-directory))
      (with-current-buffer (find-file org-file)
	(if worg-publish-stop-on-error
            (org-html-export-to-html)
          (condition-case err
	      (org-html-export-to-html)
            (error (message (error-message-string err)))))))))
