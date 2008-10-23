(add-to-list 'load-path (expand-file-name "tools"))
(require 'ert)

(defun org-export-as-html-test-fixture (file body)
  "Export the example.org buffer to html, then run the tests in
body passing in the org-buffer and the html-buffer as arguments"
  (unwind-protect
      (let* ((org-buffer (save-excursion (find-file file)
					 (buffer-name)))
	     (html-buffer "*Org HTML Export*"))
	;; setup
	(set-buffer org-buffer)
	(call-interactively 'org-export-as-html-to-buffer)
	(save-excursion
	  ;; run the tests
	  (eval (list body org-buffer html-buffer))))
    (progn
      ;; clean up
      (kill-buffer "*Org HTML Export*")
      (kill-buffer file))))

(defun org-test-search-map-all-org-html-links (buffer body)
  "for each org link in BUFFER call BODY passing the link-url and
link-text as arguments."
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\([^][]+\\)\\]\\(\\[\\([^][]+\\)\\]\\)?\\]" nil t)
      (eval (list body (match-string 1) (replace-regexp-in-string "[\r\n]" " " (match-string 3)))))))

;; this actually defines the test to be run by ert
(ert-deftest org-test-export-as-html ()
  (org-export-as-html-test-fixture
   "example.org"
   (lambda (org-buffer html-buffer)
     (org-test-search-map-all-org-html-links
      org-buffer
      (lambda (link-url link-text)
	(save-excursion
	  (message (format "%s as %s" link-url link-text))
	  (set-buffer html-buffer)
	  (goto-char (point-min))
	  (search-forward link-text)
	  (re-search-backward "<a href=\"\\(.*\\)\"" nil t)
	  (should (string= (match-string 1)
			   link-url))))))))

;; this runs the ert test
(ert-run-tests-interactively "^org-test-export-" " *org export tests*")
