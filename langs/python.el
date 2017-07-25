;;; package --- Python editing support for EMACS

;;; Commentary:

;;; Code:

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
  (flycheck-select-checker 'pycodestyle)

  (highlight-indent-guides-mode)

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
