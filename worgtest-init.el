(custom-set-variables
 '(org-modules (quote (org-jsinfo)))
 '(safe-local-variable-values 
   (quote ((org-tags-column . -80) 
	   (org-export-html-style . "<link rel=\"stylesheet\" type=\"text/css\" href=\"stylesheet.css\" />") 
	   (org-export-html-style-extra . "<link rel=stylesheet href=\"org-faq.css\" type=\"text/css\"> <style type=\"text/css\"> </style>") 
	   (org-export-html-style . "<link rel=stylesheet href=\"freeshell2.css\" type=\"text/css\"> <style type=\"text/css\"> .tag { color: red; font-weight:bold}</style>")))))

(add-to-list 'load-path "~/git/org-mode/lisp/")
(add-to-list 'load-path "~/git/org-mode/contrib/lisp/")

(show-paren-mode 1)
(menu-bar-mode 0)

(require 'org)
(require 'htmlize)

(setq org-export-in-background t)
(setq org-export-async-init-file "~/git/worg/worgtest-init.el")
(setq org-export-async-debug t)

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
      org-export-html-style-include-default nil
      org-export-htmlize-output-type 'css
      org-startup-folded nil
      org-export-allow-BIND t
      org-export-babel-evaluate nil
      org-confirm-babel-evaluate nil)
