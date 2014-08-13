(require 'gerrit)

(defun org-gerrit-update-headline (data)
  (save-excursion
    (org-gerrit-goto-current-headline)
    (re-search-forward "\\[\\[.*\\]\\[\\(.*\\)\\]\\]")
    (delete-region (point) (line-end-position))
    (insert (let ((str (concat " " (assoc-default 'subject data)))
		  (cur-col (+ (length (match-string 1)) (1+ (org-current-level)))))
	      (if (< fill-column (+ cur-col (length str)))
		  (truncate-string-to-width str (- fill-column cur-col) nil nil
					    (char-to-string (decode-char 'ucs #x2026)))
		str)))
    (org-set-tags-to (gerrit-get-patchset-tag data))))

(defvar org-gerrit-header-list '(author project branch message))

(defun org-gerrit-status (id &optional no-files)
  (let ((data (gerrit-get-patchset-data id)))
    (org-gerrit-update-headline data)
    (with-temp-buffer
      (insert (mapconcat (lambda (x) (concat (capitalize (symbol-name x))
					     ": "
					     (assoc-default x data)))
			 org-gerrit-header-list "\n"))
      (unless no-files
	(dolist (file (assoc-default 'files data))
	  (insert (format "\n%s: +%d,-%d" (car file)
			  (or (assoc-default 'lines_inserted file) 0)
			  (or (assoc-default 'lines_deleted file) 0))))
	(insert "\n"))
      (dolist (review (assoc-default 'reviews data))
	(unless (= 0 (assoc-default 'value review))
	  (insert (format "\n%s: %d" (gerrit-rec-assoc review '(name))
			  (gerrit-rec-assoc review '(value))))))
      (buffer-string))))

(defun org-gerrit-insert-patchset-headline (id &optional subheading)
  "Insert a new headline for patchset ID and show its relevant
data."
  (interactive "nPatch number: ")
  (save-excursion
    (if subheading (org-insert-subheading "") (org-insert-heading))
    (insert (org-make-link-string (format gerrit-url-fmt id)
				  (format "Patch %d" id)))
    (let ((indent (make-string (1+ (org-current-level)) ? )))
      (insert (format "\n%s#+BEGIN_SRC emacs-lisp :exports results
%s(org-gerrit-status %d)\n%s#+END_SRC" indent indent id indent)))
    (beginning-of-line)
    (org-babel-execute-src-block)
    (outline-previous-visible-heading 1)
    (org-hide-block-toggle t)
    (hide-subtree)))

(provide 'org-gerrit)
