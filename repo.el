(require 'intel-shared)

(defconst repo-buffer "*repo:command*")

(defun get-current-directory ()
  (cond ((eq major-mode 'dired-mode) (dired-current-directory))
	((expand-file-name default-directory))))


(defun untramp-path (path)
  (if (tramp-tramp-file-p path)
      (tramp-file-name-localname (tramp-dissect-file-name path))
    path))

(defun repo-upload ()
  (interactive)
  (let ((directory (get-current-directory)))
    (with-current-buffer (get-buffer-create "*repo:upload*")
      (let ((default-directory directory))
	(erase-buffer)
	(async-shell-command "repo upload ." (current-buffer))))))

(defvar repo-start-history nil)
(defun repo-start (branch)
  (interactive (list (read-string "Branch name: " nil 'repo-start-history)))
  (let ((default-directory (get-current-directory)))
    (shell-command (format "repo start %s ." branch))))

(defun repo-branches ()
  (interactive)
  (let ((default-directory (concat aosp-path "/")))
    (with-current-buffer (get-buffer-create "*repo:branch*")
      (erase-buffer)
      (call-process "repo" nil (current-buffer) nil "branches")
      (goto-char (point-min))
      (let ((l '()))
	(while (re-search-forward " \\\([0-9a-zA-Z/]+\\\) +\|" nil t)
	  (push (match-string 1) l))
	l))))

(defun repo-abandon (branch)
  (interactive (list (ido-completing-read "Branch: " (repo-branches) nil t)))
  (let ((default-directory (concat aosp-path "/")))
    (and (y-or-n-p (format "Are you sure you want to abandon %s branch ?" branch))
	 (shell-command (concat "repo abandon " branch)))))

(defun repo-sync ()
  (interactive)
  (let ((default-directory aosp-path))
    (compile (format "repo sync --force-broken -j%d" aosp-thread-number))))

(defun repo-sync-local ()
  (interactive)
  (compile (format "cd %s && repo sync -j%d ." (untramp-path default-directory) aosp-thread-number)))

(defun repo-project-dir (project &optional manifest)
  (setf manifest (or manifest "manifest.xml"))
  (let* ((data (cdr (delete-if 'stringp (car (xml-parse-file (concat aosp-path "/.repo/" manifest))))))
	 (project-data (find project data :test 'string= :key (lambda (x) (assoc-default 'name (cadr x))))))
    (if project-data
	(assoc-default 'path (cadr project-data))
      (let ((includes (delete-if-not (curry 'eq 'include) data :key 'car))
	    path)
	(while (and includes (not path))
	  (setf path (repo-project-dir project (concat "manifests/" (assoc-default 'name (cadar includes)))))
	  (setf includes (cdr includes)))
	path))))

(defun repo-clean ()
  (interactive)
  (shell-command "make clean ; repo forall  -c 'git rebase --abort ; git clean -xdf; git reset --hard'"))

(defconst repo-status-buffer "*repo:status*")
(defun repo-status ()
  (interactive)
  (let ((directory (concat aosp-path "/")))
    (with-current-buffer (get-buffer-create repo-status-buffer)
      (let ((inhibit-read-only t)
	    (pager (getenv "PAGER")))
	(setq default-directory directory)
	(erase-buffer)
	(setenv "PAGER" "")
	(set-process-filter (start-file-process "repo" (current-buffer) "repo" "status")
			    'comint-output-filter)
	(setenv "PAGER" pager)
	(repo-status-mode)
	(toggle-read-only t)
	(pop-to-buffer (current-buffer))))))

(defconst repo-project-pattern "project \\\([a-zA-Z0-9-_/]+\\\) +")

(defsubst repo-status-branch ()
  (save-excursion
    (when (re-search-forward "branch \\\([a-zA-Z0-9-_/]+\\\)" nil t)
      (match-string 1))))

(defsubst repo-status-current ()
  (save-excursion
    (goto-char (line-beginning-position))
    (if (re-search-forward repo-project-pattern (line-end-position) t)
	(cons (match-string 1) (repo-status-branch))
      (when (re-search-backward repo-project-pattern nil t)
	(cons (match-string 1) (repo-status-branch))))))

(defun repo-status-magit-log ()
  (interactive)
  (let ((current (repo-status-current)))
    (when current
      (let ((default-directory (concat default-directory "/" (car current))))
	(magit-log)))))

(defun repo-status-magit-status ()
  (interactive)
  (let ((current (repo-status-current)))
    (when current
      (magit-status (concat default-directory "/" (car current))))))

(defun repo-status-abandon-branch ()
  (interactive)
  (let ((current (repo-status-current)))
    (when (and current (cdr current)
	       (y-or-n-p (format "Are you sure you want to abandon %s branch ?" (cdr current))))
      (let ((default-directory (concat default-directory "/" (car current))))
	(shell-command (concat "repo abandon " (cdr current)))))))

(defvar repo-status-mode-map nil
  "Keymap for repo-status major mode.")
(unless repo-status-mode-map
  (setq repo-status-mode-map (make-sparse-keymap))
  (define-key repo-status-mode-map (kbd "n") 'forward-page)
  (define-key repo-status-mode-map (kbd "p") 'backward-page)
  (define-key repo-status-mode-map (kbd "g") 'repo-status)
  (define-key repo-status-mode-map (kbd "a") 'repo-status-abandon-branch)
  (define-key repo-status-mode-map (kbd "q") 'delete-window)
  (define-key repo-status-mode-map (kbd "y") 'repo-sync)
  (define-key repo-status-mode-map (kbd "l") 'repo-status-magit-log)
  (define-key repo-status-mode-map (kbd "s") 'repo-status-magit-status))

(define-derived-mode repo-status-mode shell-mode
  "repo-status"
  "repo status major mode.
Special commands:
\\{repo-status-mode-map}"
  (make-local-variable 'page-delimiter)
  (setq page-delimiter repo-project-pattern))

(provide 'repo)
