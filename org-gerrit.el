(require 'org)
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

(defcustom org-gerrit-item-list
  '(author project branch message)
  "List of item to display."
  :group 'org-gerrit)

(defcustom org-gerrit-show-diff-stat t
  "Display the diff stat."
  :group 'org-gerrit)

(defcustom org-gerrit-show-reviews t
  "Display the reviews."
  :group 'org-gerrit)

(defvar org-gerrit-progress-reporter (make-progress-reporter "Updating patches status")
  "Internal variable to store the progress reporter object.")

(defvar org-gerrit-in-progress-count 0
  "Internal variable to store the remaining patch to update.")

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
			(let ((value (assoc-default 'value review)))
			  (unless (zerop value)
			    (throw 'tag (format "REVIEWED%s%d" (if (> value 0) "+" "")
						(assoc-default 'value review)))))))
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

(defun org-gerrit-insert-headline (data id)
  (let ((status (org-gerrit-get-patchset-tag data)))
    (delete-region (point) (line-end-position))
    (insert status " "
	    (org-make-link-string (format gerrit-url-fmt id) id)
	    " "
	    (assoc-default 'subject data))))

(defun org-gerrit-insert-items (data)
  (dolist (item  org-gerrit-item-list)
    (insert (concat (capitalize (symbol-name item))
		    ": "
		    (assoc-default item data)
		    "\n")))
  (forward-line -1))

(defun org-gerrit-insert-diff-stat (data)
  (when org-gerrit-show-diff-stat
    (dolist (file (assoc-default 'files data))
      (insert (format "\n%s: +%d,-%d" (car file)
		      (or (assoc-default 'lines_inserted file) 0)
		      (or (assoc-default 'lines_deleted file) 0))))
    (insert "\n")))

(defun org-gerrit-insert-reviews (data)
  (when org-gerrit-show-reviews
    (dolist (review (assoc-default 'reviews data))
      (unless (= 0 (assoc-default 'value review))
	(insert (format "\n%s: %d" (gerrit-rec-assoc review '(name))
			(gerrit-rec-assoc review '(value))))))))

(defun org-gerrit-update-status (&optional no-files)
  (interactive)
  (save-excursion
    (let* ((id (org-entry-get (point) "ID")))
      (when id
	(incf org-gerrit-in-progress-count)
	(progress-reporter-update org-gerrit-progress-reporter)
	(gerrit-async-get-patchset-data
	 id
	 (curry 'org-gerrit-update-status-callback (point-marker)))))))

(defun org-gerrit-update-status-callback (marker id data)
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char (marker-position marker))
      (org-narrow-to-subtree)
      (show-all)
      (org-gerrit-goto-current-headline)
      (forward-char (1+ (org-current-level)))
      (delete-region (point) (point-max))
      (org-gerrit-insert-headline data id)
      (org-entry-put (point) "ID" id)
      (org-entry-put (point) "server" gerrit-host)
      (goto-char (point-max))
      (insert "\n")
      (org-gerrit-insert-items data)
      (org-gerrit-insert-diff-stat data)
      (org-gerrit-insert-reviews data)
      (indent-region (org-entry-beginning-position) (point-max))
      (widen)
      (decf org-gerrit-in-progress-count)
      (progress-reporter-update org-gerrit-progress-reporter)
      (when (zerop org-gerrit-in-progress-count)
	(progress-reporter-done org-gerrit-progress-reporter)))))

(defun org-gerrit-insert-patchset-headline (id &optional subheading)
  "Insert a new headline for patchset ID and show its relevant
data."
  (interactive "nPatch number: ")
  (save-excursion
    (if subheading
	(org-insert-subheading "")
      (org-insert-heading))
    (org-entry-put (point) "ID" (number-to-string id))
    (org-gerrit-update-status)))

(defun org-gerrit-update-file ()
  (interactive)
  (when org-gerrit-file
    (with-current-buffer (find-file-noselect org-gerrit-file)
      (save-excursion
	(goto-char (point-min))
	(org-map-entries 'org-gerrit-update-status)))))

(defun org-gerrit-get-patch (id)
  (interactive "nPatch number: ")
  (unless (org-at-heading-p)
    (goto-char (org-entry-end-position)))
  (org-gerrit-insert-patchset-headline id))

(provide 'org-gerrit)
