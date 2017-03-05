;;; package --- Tags per mode support added

;;; Commentary:

;;; Code:
(defun ltags-dir-name ()
  "Return project tags dir."
  (concat default-directory ".tags/")
  )

(defun ltags-file-name-create (lang)
  "Return tags file name for given LANG."
  (concat (ltags-dir-name) lang)
  )

(defun ltags-create-dir ()
  "Create tags directory."
  (when (not (file-exists-p (ltags-dir-name)))
    (mkdir (ltags-dir-name))
    )
  )

(defvar ltags-update-buffer-name "*tags-update*")
(defvar ltags-exec "/usr/bin/ctags")

(defvar-local ltags-lang nil "Tags language for mode.")
(defvar-local ltags-lang-tags-file nil "Tags language file.")
(defvar-local ltags-exec-opts nil "ctags options.")

(defun ltags-add-exclude-pattern (pattern)
  "Create exclude options for ctags using PATTERN."
  (concat "--exclude=" pattern)
  )

(defun ltags-create-update-cmd()
  (defvar-local ltags-exec-base-opts
    (list
     ltags-exec
     (concat "--options=" (getenv "HOME") "/.emacs.d/ctags_conf")
     (concat "--languages=" ltags-lang)
     "-f" ltags-lang-tags-file
     "-R" "-e"
     ))
  (if ltags-exec-opts
      (append ltags-exec-base-opts ltags-exec-opts)
      ltags-exec-base-opts
    )
  )


(defun ltags-update ()
  "Update tags - ARGS shall be empty."
  (interactive)
  (message "Updating tags: %s" ltags-lang)
  (make-process
   :name "tags_udate"
   :buffer (get-buffer-create
            ltags-update-buffer-name)
   :command (ltags-create-update-cmd)
   :sentinel (lambda (proc change)
               (when (string-match "\\(finished\\|exited\\)" change)
                 (kill-buffer ltags-update-buffer-name)
                 (message "Tags updated for language: %s (stored in: %s)."
                          ltags-lang
                          ltags-lang-tags-file)
                 ))
   )
  )

(defun ltags-setup (lang)
  "Setup tags for mode with given LANG."
  (setq-local ltags-lang lang)
  (setq-local ltags-lang-tags-file (ltags-file-name-create lang))
  (ltags-create-dir)
  (ltags-update)
  (setq-local tags-file-name nil)
  (setq-local tags-table-list (list ltags-lang-tags-file))
  )

(provide 'ltags)
;;; ltags.el ends here
