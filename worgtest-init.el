(mapc (lambda (e)
	(add-to-list 'load-path e))
      worg-add-load-path)

(require 'org)
(require 'htmlize)

;; to have things work correctly in batch-mode
(require 'font-lock)
(require 'cc-mode)
(c-after-font-lock-init)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-modules (quote (org-jsinfo)))
 '(safe-local-variable-values (quote ((org-tags-column . -80) (org-export-latex-image-default-option . "width=30em") (org-html-head . "<link rel=\"stylesheet\" type=\"text/css\" href=\"stylesheet.css\" />") (org-html-head-extra . "<link rel=stylesheet href=\"org-faq.css\" type=\"text/css\"> <style type=\"text/css\"> </style>") (org-export-publishing-directory . "tmp") (org-html-head . "<link rel=stylesheet href=\"freeshell2.css\" type=\"text/css\"> <style type=\"text/css\"> .tag { color: red; font-weight:bold}</style>")))))

(show-paren-mode 1)
(menu-bar-mode 0)
(set-face-foreground 'font-lock-keyword-face "DeepSkyBlue1")
(set-face-foreground 'font-lock-string-face "pale goldenrod")

(setq make-backup-files nil
      vc-handled-backends nil)

(setq org-export-default-language "en"
      org-export-html-extension "html"
      org-export-with-timestamps nil
      org-export-with-section-numbers nil
      org-export-with-tags 'not-in-toc
      org-export-skip-text-before-1st-heading nil
      org-export-with-sub-superscripts '{}
      org-export-with-LaTeX-fragments t
      org-export-with-archived-trees nil
      org-export-highlight-first-table-line t
      org-export-latex-listings-w-names nil
      org-export-html-style-include-default nil
      org-export-htmlize-output-type 'css
      org-startup-folded nil
      org-export-allow-BIND t
      org-publish-list-skipped-files t
      org-publish-use-timestamps-flag t
      org-export-babel-evaluate nil
      org-confirm-babel-evaluate nil)

;; re-export everything regardless of whether or not it's been modified
(setq org-publish-use-timestamps-flag nil)

(defun set-org-publish-project-alist ()
  (interactive)
  (setq org-publish-project-alist
	`(("worg" :components ("worg-org-faq" "worg-pages" "worg-code" "worg-color-themes" "worg-images-dir" "worg-images" "worg-sources" "worg-extra" "worg-bibtex"))
	  ("worg-org-faq"
	   :base-directory ,worg-base-directory
	   :base-extension "dummy"
	   :include ("org-faq.org")
	   :html-extension "html"
	   :publishing-directory ,worg-publish-directory
	   :publishing-function org-html-publish-to-html
	   :section-numbers nil
	   :table-of-contents nil
	   :style "<link rel=\"stylesheet\" title=\"Standard\" href=\"/worg/style/worg.css\" type=\"text/css\" />
<link rel=\"alternate stylesheet\" title=\"Zenburn\" href=\"/worg/style/worg-zenburn.css\" type=\"text/css\" />
<link rel=\"alternate stylesheet\" title=\"Classic\" href=\"/worg/style/worg-classic.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"https://orgmode.org/css/lightbox.css\" type=\"text/css\" media=\"screen\" />
<link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\" />"
	   :recursive t
	   :html-preamble ,(org-get-file-contents (concat worg-base "preamble.html"))
	   :html-postamble "<div id=\"show_source\"><input type=\"button\" value=\"Show Org source\" onClick='show_org_source()'></div><div id=\"license\"><p>Documentation from the https://orgmode.org/worg/ website (either in its HTML format or in its Org format) is licensed under the <a href=\"http://www.gnu.org/copyleft/fdl.html\">GNU Free Documentation License version 1.3</a> or later.  The code examples and css stylesheets are licensed under the <a href=\"http://www.gnu.org/licenses/gpl.html\">GNU General Public License v3</a> or later.</p></div>"
	   )
	  ("worg-pages"
	   :base-directory ,worg-base-directory
	   :base-extension "org"
	   :exclude "FIXME"
	   :makeindex t
	   :auto-sitemap nil
	   :sitemap-ignore-case t
	   :html-extension "html"
	   :publishing-directory ,worg-publish-directory
 	   :publishing-function (org-html-publish-to-html)
	   :htmlized-source nil
	   :section-numbers nil
	   :table-of-contents nil
	   :style "<link rel=\"stylesheet\" title=\"Standard\" href=\"/worg/style/worg.css\" type=\"text/css\" />
<link rel=\"alternate stylesheet\" title=\"Zenburn\" href=\"/worg/style/worg-zenburn.css\" type=\"text/css\" />
<link rel=\"alternate stylesheet\" title=\"Classic\" href=\"/worg/style/worg-classic.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"https://orgmode.org/css/lightbox.css\" type=\"text/css\" media=\"screen\" />
<link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\" />"
	   :recursive t
	   :html-preamble ,(org-get-file-contents (concat worg-base "preamble.html"))
	   :html-postamble "<div id=\"show_source\"><input type=\"button\" value=\"Show Org source\" onClick='show_org_source()'></div><div id=\"license\"><p>Documentation from the https://orgmode.org/worg/ website (either in its HTML format or in its Org format) is licensed under the <a href=\"http://www.gnu.org/copyleft/fdl.html\">GNU Free Documentation License version 1.3</a> or later.  The code examples and css stylesheets are licensed under the <a href=\"http://www.gnu.org/licenses/gpl.html\">GNU General Public License v3</a> or later.</p></div>"
	   )
	  ("worg-code"
	   :base-directory ,worg-base-code-directory
	   :base-extension "html\\|css\\|png\\|js\\|bz2\\|el\\|sty\\|awk\\|pl"
	   :html-extension "html"
	   :publishing-directory ,(concat worg-htmlroot "/code/")
	   :recursive t
	   :publishing-function org-publish-attachment)
	  ("worg-color-themes"
	   :base-directory ,worg-base-color-themes-directory
	   :base-extension "el"
	   :html-extension "html"
	   :publishing-directory ,(concat worg-htmlroot "/code/elisp/")
	   :recursive t
	   :publishing-function org-publish-attachment)
	  ("worg-images-dir"
	   :base-directory ,worg-base-images-directory
	   :base-extension "png\\|jpg\\|gif\\|pdf\\|cvs\\|css"
	   :publishing-directory ,(concat worg-htmlroot "/images/")
	   :recursive t
	   :publishing-function org-publish-attachment)
	  ("worg-images"
	   :base-directory ,worg-base-directory
	   :base-extension "png\\|jpg\\|gif\\|pdf\\|csv\\|css\\|tex"
	   :publishing-directory ,worg-publish-directory
	   :recursive t
	   :publishing-function org-publish-attachment)
	  ("worg-sources"
	   :base-directory ,worg-base-directory
	   :base-extension "org"
	   :publishing-directory ,(concat worg-htmlroot "/sources/")
	   :recursive t
	   :publishing-function org-publish-attachment)
	  ("worg-extra"
	   :base-directory ,worg-base-directory
	   :base-extension "css"
	   :publishing-directory ,worg-publish-directory
	   :publishing-function org-publish-attachment)
	  ("worg-bibtex"
	   :base-directory ,(concat worg-base "org-contrib/bibtex/")
	   :base-extension "bib"
	   :publishing-directory ,(concat worg-htmlroot "/org-contrib/bibtex/")
	   :recursive nil
	   :publishing-function org-publish-attachment)
	  )))

(setq worg-base-directory worg-base)
(setq worg-base-code-directory (concat worg-base "code/"))
(setq worg-base-color-themes-directory (concat worg-base "code/elisp/"))
(setq worg-base-images-directory (concat worg-base "images/"))
(setq worg-publish-directory worg-htmlroot)
(set-org-publish-project-alist)

(defun worg-fix-symbol-table ()
  (when (string-match "org-symbols\\.html" buffer-file-name)
    (goto-char (point-min))
    (while (re-search-forward "<td>&amp;\\([^<;]+;\\)" nil t)
      (replace-match (concat "<td>&" (match-string 1)) t t))))

(defun publish-worg-old nil
   "Publish Worg."
   (interactive)
   (add-hook 'org-publish-after-export-hook 'worg-fix-symbol-table)
   (let ((org-format-latex-signal-error nil)
	 (worg-base-directory worg-base)
	 (worg-base-code-directory (concat worg-base "code/"))
	 (worg-base-color-themes-directory (concat worg-base "code/elisp/"))
	 (worg-base-images-directory (concat worg-base "images/"))
	 (worg-publish-directory worg-htmlroot))
     (set-org-publish-project-alist)
     (org-publish-project "worg")))

(defun publish-worg nil
   "Publish Worg."
   (interactive)
   (add-hook 'org-publish-after-export-hook 'worg-fix-symbol-table)
   (let ((org-format-latex-signal-error nil)
	 (worg-base-directory worg-base)
	 (worg-base-code-directory (concat worg-base "code/"))
	 (worg-base-color-themes-directory (concat worg-base "code/elisp/"))
	 (worg-base-images-directory (concat worg-base "images/"))
	 (worg-publish-directory worg-htmlroot))
     (set-org-publish-project-alist)
     (message "Emacs %s" emacs-version)
     (org-version)
     (org-publish-project "worg")))

(defun parse-org-quotes ()
  "Create ~/orgmode.org/org-quotes.js from org-quotes.org."
  (interactive)
  (load (concat worg-base "code/elisp/worg-fortune.el"))
  (worg-write-fortune-file
   (concat worg-base "org-quotes.org")
   "~/orgmode.org/org-quotes.js"
   120
   "r_text[%d] = \"%s\";" "\n"
   'worg-fortune-insert-javascript-pre
   'worg-fortune-insert-javascript-post))
