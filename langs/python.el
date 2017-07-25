;;; package --- Python editing support for EMACS

;;; Commentary:

;;; Code:

(flycheck-define-checker python-pycodestyle
  "Python codestyle checker"
  :command ("pycodestyle" "-")
  :error-patterns
  ((error line-start "stdin:" line ":" column ": E" (id) (message) line-end)
   (warning line-start "stdin:" line ":" column ": W" (id) (message) line-end))
  :modes (python-mode)
  :next-checkers (python-pylint)
  :standard-input t)


(use-package anaconda)
(use-package company-jedi)

(defun python/mode-setup()
  "Setup function for python mode"
 
   (set (make-local-variable 'company-backends)
        '((
           company-files
           company-yasnippet
           company-jedi
           )))

  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-mode)
  (flycheck-select-checker 'python-pycodestyle)
  (flycheck-add-next-checker 'python-pycodestyle 'python-pylint)

  ;; (highlight-indent-guides-mode)

  (anaconda-mode)
  (anaconda-eldoc-mode)
  (company-mode)
  (company-quickhelp-mode 1)
)

(add-hook 'python-mode-hook 'python/mode-setup)

(evil-leader/set-key-for-mode
  'python-mode
  "d" 'anaconda-mode-show-doc
  "j" 'anaconda-mode-find-definitions
  "r" 'anaconda-mode-find-references)

(provide 'python)
;;; python.el ends here
