
(require 'anaconda-mode)
(require 'company-anaconda)
(require 'flycheck-pyflakes)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(defun python/mode-setup()
  "Setup function for python mode"
  (setq-local company-backends
              '(company-anaconda
                company-yasnippet
                company-dabbrev
                company-quickhelp))
  (flycheck-mode)
  (flycheck-select-checker 'python-flake8)

  (local-set-key (kbd "C-c d") 'anaconda-mode-show-doc)

  (highlight-indent-guides-mode)
)

(add-hook 'python-mode-hook 'python/mode-setup)

(evil-leader/set-key-for-mode
  'python-mode
  "s d" 'anaconda-mode-show-doc
  "g d" 'anaconda-mode-find-definitions
  "f r" 'anaconda-mode-find-references
  "g b" 'anaconda-mode-go-back)
