;;; package --- Tags per mode support added

;;; Commentary:

;;; Code:
(defun ltags-dir-name ()
  "Return project tags dir."
  (concat default-directory ".tags/")
  )

(defun ltags-file-name (lang)
  "Return tags file name for given LANG."
  (concat (ltags-dir-name) lang)
  )

(defun ltags-create-dir ()
  "Create tags directory."
  (when (not (file-exists-p (ltags-dir-name)))
    (mkdir (ltags-dir-name))
    )
  )

(defvar-local ltags-update-buffer-name "*tags-update*")
(defvar-local ltags-exec "ctags")

(defvar-local ltags-lang nil "Tags language for mode.")
(defvar-local ltags-lang-file nil "Tags language file.")
(defvar-local ltags-exec-opts nil "ctags options.")

(defun ltags-add-exclude-pattern (pattern)
  "Create exclude options for ctags using PATTERN."
  (concat "--exclude=" pattern)
  )

(defun ltags-create-update-opts(lang filename user-options)
  (defvar-local ltags-exec-base-opts
    (list
     "--langmap=make:([Mm]akefile).conf([Mm]akefile).variables"
     (concat"--languages=" lang)
     "-f" filename
     "-R" "-e"
     (ltags-add-exclude-pattern ".git/*")
     (ltags-add-exclude-pattern ".svn/*")
     ))
  (if user-options
      (append ltags-exec-base-opts user-options)
      ltags-exec-base-opts
    )
  )

;;;###autoload
(defun ltags-update (&optional args)
  "Update tags - ARGS shall be empty."
  (interactive "P")
  (defvar-local process (apply 'start-process
                "tags update"
                ltags-update-buffer-name
                ltags-exec
                (ltags-create-update-opts ltags-lang ltags-lang-file ltags-exec-opts)
                ))
  (set-process-sentinel
   process
   (lambda (proc change)
      (when (string-match "\\(finished\\|exited\\)" change)
        (kill-buffer ltags-update-buffer-name)
        (message (concat "Tags updated for language: " ltags-lang " (stored in: " ltags-lang-file ")"))
        )))
  )
;;;###autoload
(defun ltags-setup (lang)
  "Setup tags for mode with given LANG."
  (setq-local ltags-lang lang)
  (setq-local ltags-lang-file (ltags-file-name lang))
  (ltags-create-dir)
  (ltags-update)
  (message "lang tags file")
  (setq tags-file-name nil)
  (setq tags-table-list (list ltags-lang-file))
  )

(provide 'ltags)
;;; ltags.el ends here
