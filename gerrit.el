(require 'repo)
(require 'json)
(require 'auth-source)

(defvar gerrit-confs '())

(defstruct gerrit-conf
  name
  host
  protocol
  port
  secured)

(defconst gerrit-log-buffer "*gerrit-log*")

(defconst gerrit-curl-cmd "curl")

(defconst gerrit-cookie (make-temp-file "gerrit-cookie"))

(defconst gerrit-host "android.intel.com")
(defconst gerrit-protocol "https")
(defconst gerrit-port "443")
(defconst gerrit-url (concat gerrit-protocol "://" gerrit-host
			     (when gerrit-port (concat ":" gerrit-port))))
(defconst gerrit-secured nil)

(defconst gerrit-url-fmt (format "%s/%%s" gerrit-url))

(defconst gerrit-curl-args-fmt
  (concat "-b " gerrit-cookie " -c " gerrit-cookie
	  (if gerrit-secured "" " --insecure") " \
  -s -H Accept:application/json,application/jsonrequest \
" gerrit-url "/changes/?q=%s&o=CURRENT_REVISION\
&o=CURRENT_COMMIT&o=CURRENT_FILES&o=DETAILED_LABELS"))

(defconst gerrit-curl-cookie-fmt
  (concat "-c " gerrit-cookie (if gerrit-secured "" " --insecure") " \
-d username=%s&password=%s " gerrit-url "/login"))

(defconst gerrit-buf-fmt " *gerrit:%s*")

(defun gerrit-error (string)
    (gerrit-log (concat "Error: " string))
    (error string))

(defun gerrit-log (string)
  (with-current-buffer (get-buffer-create gerrit-log-buffer)
    (end-of-buffer)
    (insert (concat string "\n")))
  (redisplay))

(defun gerrit-authentificate ()
  (let ((token (car (auth-source-search :host gerrit-host :port "gerrit"
					:create t :max 1))))
    (when (file-exists-p gerrit-cookie)
      (delete-file gerrit-cookie))
    (apply 'call-process gerrit-curl-cmd nil nil nil
	   (split-string (format gerrit-curl-cookie-fmt
				 (plist-get token :user)
				 (funcall (plist-get token :secret)))))
    (unless (file-exists-p gerrit-cookie)
      (gerrit-error "Gerrit authentification failed"))))

(defun gerrit-rec-assoc (json names)
  (if names
      (gerrit-rec-assoc (assoc (car names) json) (cdr names))
    (cdr json)))

(defun gerrit-id (json)
  "Returns the patch ID."
  (gerrit-rec-assoc json '(_number)))

(defun gerrit-changeid (json)
  "Returns the patch change ID"
  (gerrit-rec-assoc json '(change_id)))

(defun gerrit-author (json)
  "Returns the patch author."
  (gerrit-rec-assoc json '(owner name)))

(defun gerrit-status (json)
  "Returns the patch status."
  (gerrit-rec-assoc json '(status)))

(defun gerrit-subject (json)
  "Returns the patch subject."
  (gerrit-rec-assoc json '(subject)))

(defun gerrit-message (json)
  "Returns the patch commit message."
  (gerrit-rec-assoc (cdadr (assoc 'revisions json)) '(commit message)))

(defun gerrit-project (json)
  "Returns the patch project."
  (gerrit-rec-assoc json '(project)))

(defun gerrit-branch (json)
  "Returns the patch branch."
  (gerrit-rec-assoc json '(branch)))

(defun gerrit-files (json)
  "Returns files impacted by the patch."
  (gerrit-rec-assoc (cdadr (assoc 'revisions json)) '(files)))

(defun gerrit-ssh (json)
  "Returns ssh data to build the fetch command."
  (let ((data (gerrit-rec-assoc (cdadr (assoc 'revisions json)) '(fetch ssh))))
    (setf (cdr (assoc 'url data))
	  (replace-regexp-in-string "^ssh://\\*" (concat "ssh://" gerrit-host)
				    (assoc-default 'url data)))
    data))

(defun gerrit-reviews (json)
  "Returns the reviews of this patch."
  (sort (mapcar (lambda (x) (append x nil))
		(gerrit-rec-assoc json '(labels Code-Review all)))
	(lambda (x y)
	  (string< (assoc-default 'name x) (assoc-default 'name y)))))

(defconst gerrit-default-data-list '(id changeid author status
				     subject message project
				     ssh branch reviews files))

(defun gerrit-call (sym json)
  (funcall (intern (concat "gerrit-" (symbol-name sym))) json))

(defun gerrit-buf-to-data (&optional data-list)
  (beginning-of-buffer)
  (let ((data-list (or data-list gerrit-default-data-list))
	(json (json-read-from-string
	       (replace-regexp-in-string
		"\"" "\""
		(buffer-substring (progn (search-forward "{")
					 (match-beginning 0))
				  (point-max)) nil t))))
    (kill-buffer (current-buffer))
    (mapcar (lambda (x) (cons x (gerrit-call x json))) data-list)))

(defsubst gerrit-do-get-patchset-data (id data-list)
  (apply 'call-process gerrit-curl-cmd nil t nil
	 (split-string (format gerrit-curl-args-fmt id)))
  (gerrit-buf-to-data data-list))

(defun gerrit-get-patchset-data (id &optional data-list)
  "Retrieves ID patchset data synchronously (could take a while...)."
  (with-current-buffer (get-buffer-create (format gerrit-buf-fmt id))
    (condition-case nil
	(gerrit-do-get-patchset-data id data-list)
      (error (when (y-or-n-p "Gerrit data retrieval failed, do you\
 want to authentificate first and re-try then ?")
	       (call-interactively 'gerrit-authentificate)
	       (gerrit-do-get-patchset-data id data-list))))))

(defun gerrit-async-get-patchset-data (id callback &optional data-list)
  "Retrieves ID gerrit patch data asynchronously. The CALLBACK
function prototype is foo(id data)."
  (with-current-buffer (get-buffer-create (format gerrit-buf-fmt id))
    (set-process-sentinel
     (apply 'start-process gerrit-curl-cmd (current-buffer)
	    gerrit-curl-cmd (split-string (format gerrit-curl-args-fmt id)))
     (curry 'gerrit-sentinel id callback data-list))))

(defun gerrit-sentinel (id callback data-list p e)
  (with-current-buffer (process-buffer p)
    (if (string= e "finished\n")
	(funcall callback id (gerrit-buf-to-data data-list))
      (gerrit-error "gerrit patch %d data retrieval failed." id))))

(defsubst gerrit-current-id ()
  (or (and (eq major-mode 'gerrit-show-mode) gerrit-buf-id)
      (and (eq major-mode 'org-mode) (org-gerrit-current-id))
      (number-at-point)))

;; AOSP interaction
(defconst gerrit-buffer "*gerrit*")

(defsubst gerrit-call-process (msg cmd &rest args)
  (let ((exec-directory default-directory))
    (with-current-buffer (get-buffer-create gerrit-buffer)
      (save-excursion
	(let ((default-directory exec-directory))
	  (goto-char (point-max))
	  (when msg
	    (insert msg "\n"))
	  (or (= 0 (apply 'process-file cmd nil (current-buffer) nil args))
	      (gerrit-error (concat "The following command failed: " cmd " "
			     (mapconcat 'identity args " ")))))))))

(defun gerrit-fetch (id &optional patchset-data)
  "Fetch patch number ID. On success, the directory in which the
patch has been fetched is returned. PATCHSET-DATA, when provided,
must contains project and ssh parts from
`gerrit-get-patchset-data'."
  (interactive "nPatch number: ")
  (let* ((data (or patchset-data (gerrit-get-patchset-data id '(project ssh))))
	 (ssh (assoc 'ssh data))
	 (project-dir (repo-project-dir (assoc-default 'project data)))
	 (default-directory (concat aosp-path "/" project-dir "/")))
    (unless project-dir
      (error "Project %s path not found." (assoc-default 'project data)))
    (gerrit-call-process (format "Fetching %d patch." id)
			 "git" "fetch" (assoc-default 'url ssh)
			 (assoc-default 'ref ssh))
    default-directory))

(defsubst gerrit-read-branch ()
  (read-string "Branch (empty means no new branch): " nil 'repo-start-history))

(defun gerrit-apply (id &optional branch)
  "Apply gerrit patch ID. When BRANCH is provided, a new branch
named BRANCH will be created before applying this patch."
  (interactive (list (read-number "Patch number: " (gerrit-current-id))
		     (gerrit-read-branch)))
  (pop-to-buffer gerrit-log-buffer)
  (gerrit-log (format "\n* Applying patch %d on branch %s." id branch))
  (let ((default-directory (gerrit-fetch id)))
    (gerrit-log (format "Project: %s." default-directory))
    (unless (string= branch "")
      (gerrit-call-process nil "~/bin/repo" "start" branch "."))
    (gerrit-call-process (format "Check-pick patch %d." id)
			 "git" "cherry-pick" "FETCH_HEAD")
    (let ((msg (format "Patch %d has been successfully applied." id)))
      (gerrit-log msg)
      (message msg))))

(defun gerrit-find-file (&optional other-window)
  "Visit the project of a gerrit patch.
With a prefix argument, visit in other window."
  (interactive "P")
  (let* ((id (read-number "Patch number: " (gerrit-current-id)))
	 (data (gerrit-get-patchset-data id '(project)))
	 (ido-use-filename-at-point nil)
	 (project-dir (repo-project-dir (assoc-default 'project data)))
	 (default-directory (concat aosp-path "/" project-dir)))
    (if other-window
	(ido-find-file-other-window)
      (ido-find-file))))

(defun gerrit-show (&optional other-window)
  "Visit a gerrit patch.
With a prefix argument, visit in other window."
  (interactive "P")
  (let* ((id (read-number "Patch number: " (gerrit-current-id)))
	 (data (gerrit-get-patchset-data id))
	 (exec-directory (gerrit-fetch id)))
    (with-current-buffer (get-buffer-create (format "*gerrit-patch:%d*" id))
      (let ((default-directory exec-directory)
	    (inhibit-read-only t))
	(erase-buffer)
	(save-excursion
	  (insert "Branch: " (assoc-default 'branch data) "\n")
	  (insert "Status: " (assoc-default 'status data) "\n")
	  (insert "Project: " (assoc-default 'project data) "\n")
	  (process-file "git" nil (current-buffer) nil "show" "FETCH_HEAD")
	  (gerrit-show-mode)
	  (setq gerrit-buf-id id)
	  (if other-window
	      (pop-to-buffer (current-buffer))
	    (pop-to-buffer-same-window (current-buffer)))))
      (toggle-read-only t))))

(defvar-local gerrit-buf-id nil)

(defvar gerrit-show-mode-map nil
  "Keymap for gerrit-show major mode.")
(unless gerrit-show-mode-map
  (setq gerrit-show-mode-map (make-sparse-keymap))
  (define-key gerrit-show-mode-map (kbd "C-c C-a") 'gerrit-apply)
  (define-key gerrit-show-mode-map (kbd "C-c C-f") 'gerrit-find-file)
  (define-key gerrit-show-mode-map (kbd "C-c C-o") 'gerrit-open)
  (define-key gerrit-show-mode-map (kbd "q") 'kill-buffer-and-window))

(define-derived-mode gerrit-show-mode diff-mode
  "gerrit-show"
  "gerrit-show major mode.
Special commands:
\\{gerrit-show-mode-map}")

(defun gerrit-search-patch (query)
  "Search gerrit patch based on a string (change id, patch number, sha1, ...)"
  (interactive (list (read-string "Search: " (substring-no-properties (word-at-point)))))
  (cdar (gerrit-get-patchset-data query '(id))))

;; Open in web browser
(defun gerrit-open (query)
  "Search gerrit patch based on a string a open it in web browser"
  (interactive (list (read-string "Browse patch: " (substring-no-properties (word-at-point)))))
  (browse-url (format gerrit-url-fmt (gerrit-search-patch query))))

;; Other tools
(defun gerrit-apply-patch-list (&optional branch)
  "Looks for the gerrit patch link in the select selected region
and call `gerrit-apply' function with BRANCH parameter.  If no
region is selected it will apply on the entire buffer."
  (interactive (list (gerrit-read-branch)))
  (let ((start (if (region-active-p) (region-beginning) (point-min)))
	(end (if (region-active-p) (region-end) (point-max))))
    (when (or (region-active-p)
	      (y-or-n-p "No selected region, Are you sure you want to apply on the entire buffer ?"))
      (while (re-search-forward (concat gerrit-url "/\\\([0-9]+\\\)") nil t)
	(save-excursion
	  (gerrit-apply (string-to-number (match-string 1)) branch))))))

(provide 'gerrit)
