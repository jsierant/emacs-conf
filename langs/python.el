;;; package --- Python editing support for EMACS

;;; Commentary:

;;; Code:


(require 'anaconda-mode)
(require 'company-anaconda)
(require 'flycheck-pyflakes)
(require 'company-quickhelp)

(defun python/mode-setup()
  "Setup function for python mode"
   (set (make-local-variable 'company-backends)
        '((company-anaconda
           company-yasnippet
           company-files
           company-dabbrev
           )))

  (flycheck-mode)
  (flycheck-select-checker 'python-flake8)

  (highlight-indent-guides-mode)

  (anaconda-mode)
  (anaconda-eldoc-mode)
  (local-set-key (kbd "C-c d") 'anaconda-mode-show-doc)

  (company-mode)
  (company-quickhelp-mode 1)
)

(add-hook 'python-mode-hook 'python/mode-setup)

(evil-leader/set-key-for-mode
  'python-mode
  "s d" 'anaconda-mode-show-doc
  "g d" 'anaconda-mode-find-definitions
  "f r" 'anaconda-mode-find-references
  "g b" 'anaconda-mode-go-back
  "r c" 'py-autopep8-buffer)

(provide 'python)
;;; python.el ends here
