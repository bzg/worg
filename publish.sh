#!/usr/bin/env sh
":"; # -*- mode: emacs-lisp; lexical-binding: t; -*-
":"; exec emacs --no-init-file --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(defvar worg-publish-stop-on-error (member "--debug" command-line-args)
  "When non-nil, stop publishing process when an error is encountered.
This variable can be set when running publish.sh script:
  ./publish.sh --debug")

(push "~/org-mode/lisp/" load-path)
(setq load-prefer-newer t)
(require 'ox-html)
(require 'cl-seq)
(require 'htmlize)
(require 'org-inlinetask)

(setq make-backup-files nil
      debug-on-error t)

(push '(:eval . "no-export") org-babel-default-header-args)
(push '(:eval . "no-export") org-babel-default-inline-header-args)

;; FIXME: Working around ESS bug.  `font-lock-reference-face' has been removed in Emacs 29.
(define-obsolete-variable-alias
  'font-lock-reference-face 'font-lock-constant-face "20.3")

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

;; Force default implementation to avoid interactive prompts.
;; See https://builds.sr.ht/~bzg/job/1037325
(require 'geiser)
(setq geiser-default-implementation 'racket)

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
	     (file-newer-than-file-p html-file org-file)
             ;; If there are include files or code, we need to
             ;; re-generate the HTML just in case if the included
             ;; files are changed.
             (with-temp-buffer
               (insert-file-contents org-file)
               (and
		(save-excursion
		  (goto-char (point-min))
		  (not (re-search-forward "#\\+include:" nil t)))
                (save-excursion
		  (goto-char (point-min))
		  (not (re-search-forward "#\\+begin_src" nil t))))))
	(message " [skipping] unchanged %s" org-file)
      (message "[exporting] %s" (file-relative-name org-file default-directory))
      (with-current-buffer (find-file org-file)
	(if worg-publish-stop-on-error
            (org-html-export-to-html)
          (condition-case err
	      (org-html-export-to-html)
            (error (message (error-message-string err)))))))))
