;;; package --- Python editing support for EMACS

;;; Commentary:

;;; Code:


(require 'flycheck-pyflakes)
(require 'company-quickhelp)
(require 'company-jedi)
(require 'elpy)

(defun python/mode-setup()
  "Setup function for python mode"
   (set (make-local-variable 'company-backends)
        '((company-jedi
           company-yasnippet
           company-files
           company-dabbrev
           )))

  (flycheck-mode)
  (flycheck-select-checker 'python-flake8)

  (highlight-indent-guides-mode)

  (elpy-mode)

  (local-set-key (kbd "C-c d") 'elpy-doc)

  (setq-local completion-styles "substring")

  (company-mode)
  (company-quickhelp-mode 1)
)

(add-hook 'python-mode-hook 'python/mode-setup)

(evil-leader/set-key-for-mode
  'python-mode
  "s d" 'elpy-doc
  "h d" (lambda () (interactive "")
          (switch-to-buffer "*Python Doc*")
          (kill-buffer-and-window))

  "g d" 'elpy-goto-definition
  "f r" 'elpy-rgrep-symbol
  "g b" 'pop-tag-mark
  "r c" 'py-autopep8-buffer)

(evil-define-key
  'normal python-mode-map
  (kbd "K") 'elpy-nav-backward-block
  (kbd "J") 'elpy-nav-forward-block)


(remove-hook 'elpy-modules 'elpy-module-flymake)
(remove-hook 'elpy-modules 'elpy-module-company)
(remove-hook 'elpy-modules 'elpy-module-yasnippet)
(remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

(elpy-enable)

(provide 'python)
;;; python.el ends here
