(require 'gerrit)

(defcustom org-gerrit-file nil
  "File to store org-gerrit headlines."
  :group 'org-gerrit)

(defcustom org-gerrit-ignored-reviewer-list '()
  "List of user which you don't care for their opinion.  Usually
  bots."
  :group 'org-gerrit)

(defcustom org-gerrit-my-name nil
  "Your username as it appears in gerrit web interface."
  :group 'org-gerrit)

(defun org-gerrit-goto-current-headline ()
  (if (org-at-heading-p)
      (goto-char (line-beginning-position))
    (outline-next-visible-heading -1)))

(defun org-gerrit-get-patchset-tag (data)
  (or (and (equal (assoc-default 'status data) "MERGED") "MERGED")
      (and (equal (assoc-default 'status data) "ABANDONED") "ABANDONED")
      (let ((review-score 0)
	    tag)
	(if (and org-gerrit-my-name
		 (not (string= org-gerrit-my-name (assoc-default 'author data))))
	    (catch 'tag
	      (mapc (lambda(review)
		      (when (equal (assoc-default 'name review) org-gerrit-my-name)
			(throw 'tag "REVIEWED")))
		    (assoc-default 'reviews data))
	      "TO-REVIEW")
	  (catch 'tag
	    (mapc (lambda(review)
		    (unless (member (assoc-default 'name review) org-gerrit-ignored-reviewer-list)
		      (when (equal (assoc-default 'value review) 2)
			(throw 'tag "APPROVED"))
		      (when (= (assoc-default 'value review) -1)
			(throw 'tag "REVIEW-1"))
		      (when (< (assoc-default 'value review) -1)
			(throw 'tag "REFUSED"))
		      (unless (equal (assoc-default 'name review) (assoc-default 'author data))
			(setq review-score (+ (assoc-default 'value review) review-score)))))
		  (assoc-default 'reviews data))
	    (if (zerop review-score)
		"IN-REVIEW"
	      (concat "REVIEW+" (number-to-string review-score))))))))

(defun org-gerrit-update-headline (data status)
  (save-excursion
    (org-gerrit-goto-current-headline)
    (forward-char (1+ (org-current-level)))
    (delete-region (point) (line-end-position))
    (insert status " "
	    (org-make-link-string (format gerrit-url-fmt id)
				  (format "Patch %d" id))
	    " "
	    (assoc-default 'subject data))))

(defvar org-gerrit-header-list '(author project branch message))

(defun org-gerrit-status (id &optional no-files)
  (let ((data (gerrit-get-patchset-data id)))
    (org-gerrit-update-headline data (org-gerrit-get-patchset-tag data))
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
