;;; package --- CMake editing support for Emacs
;;; Commentary:

;;; Code:

(require 'company)
(require 'cmake-mode)
(require 'cmake-font-lock)


;;;###autoload
(defun cmake/mode-setup()
  "Setup function for cmake mode"
   (set (make-local-variable 'company-backends)
        '((company-cmake
           :with company-etags
           :with company-yasnippet
           :with company-files
           :with company-dabbrev
           )))

   (local-set-key (kbd "<f5>") 'ltags-update)
   (ltags-setup "cmake")

   (company-mode 1)
   (cmake-font-lock-activate)
)

(add-hook 'cmake-mode-hook 'cmake/mode-setup)

(defun cmake-show-doc (symbol_opt symbol)
  "Shows documentation of symbol at point in buffer."
  (interactive "P")

  (defvar-local process (apply 'start-process
                "cmake doc"
                "*cmake-doc*"
                "cmake"
                 (list symbol_opt symbol)
                ))

  (set-process-sentinel
   process
   (lambda (proc change)
     (when (string-match "\\(finished\\|exited\\)" change)
       (let ((buf (get-buffer-create "*cmake-doc*")))
         (with-current-buffer buf
           (goto-char (point-min))
           buf))
        (display-buffer "*cmake-doc*")
        )))
  )

;;;###autoload
(defun cmake-doc-command-at-point (&rest args)
  "Shows documentation of symbol at point in buffer."
  (interactive "P")
  (cmake-show-doc "--help-command" (thing-at-point 'word))
  )

;;;###autoload
(defun cmake-doc-variable-at-point (&rest args)
  "Shows documentation of variable at point in buffer."
  (interactive "P")
  (cmake-show-doc "--help-variable" (thing-at-point 'word))
  )

;;;###autoload
(defun cmake-doc-module-at-point (&rest args)
  "Shows documentation of module at point in buffer."
  (interactive "P")
  (cmake-show-doc "--help-module" (thing-at-point 'word))
  )


;; (defun helm-source-cmake-commands ()
;;   "Helm source with list of cmake commands"
;;   (interactive "P")
;;   (helm-build-async-source
;;       "CMake Commands"
;;     :candidates-process
;;     (lambda ()
;;       (start-process "cmake" nil "cmake" "--help-command-list"))
;;     :action (lambda(candidate) (cmake-show-doc "--help-command" candidate))
;;     )
;;   )

;; ;;;###autoload
;; (defun helm-cmake-doc ()
;;   (interactive)
;;   (helm :sources 'helm-source-cmake-commands
;;         :buffer "*helm-cmake-doc*")
;; )


(evil-leader/set-key-for-mode
  'cmake-mode
  "d c" 'cmake-doc-command-at-point
  "d v" 'cmake-doc-variable-at-point
  "d m" 'cmake-doc-module-at-point
  "j" 'xref-find-definitions
  "w j" 'xref-find-definitions-other-window
  "u" 'xref-find-references
  "b" 'xref-pop-marker-stack
  )

(provide 'cmake)
;;; cmake.el ends here
