;;; package --- Python editing support for EMACS

;;; Commentary:

;;; Code:


(require 'flycheck-pyflakes)
(require 'company-quickhelp)
(require 'company-jedi)
(require 'elpy)


(require 'flycheck)


(flycheck-define-checker python-pycodestyle
  "Python codestyle checker"
  :command ("pycodestyle" "-")
  :error-patterns
  (
   (error line-start "stdin:" line ":" column ": E" (id) (message) line-end)
   (warning line-start "stdin:" line ":" column ": W" (id) (message) line-end)
   )
  :modes (python-mode)
  :next-checkers (python-pylint)
  :standard-input t
  )


(defun python/mode-setup()
  "Setup function for python mode"
   (set (make-local-variable 'company-backends)
        '((company-jedi
           :with company-yasnippet
           :with company-files
           :with company-dabbrev
           )))

  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-mode)
  (flycheck-select-checker 'python-pycodestyle)

  (highlight-indent-guides-mode)

  (elpy-mode)
  (eldoc-mode)

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

  "d"   'elpy-goto-definition
  "w d" 'elpy-goto-definition-other-window
  "b"   'pop-tag-mark
  "f r" 'elpy-rgrep-symbol
  "e s" 'elpy-multiedit-python-symbol-at-point
  "f c" 'elpy-format-code
  )

(evil-define-key
  'normal python-mode-map
  (kbd "K") 'elpy-nav-backward-block
  (kbd "J") 'elpy-nav-forward-block
  (kbd "C-j") 'elpy-nav-move-line-or-region-dow
  (kbd "C-k") 'elpy-nav-move-line-or-region-up
  )


(remove-hook 'elpy-modules 'elpy-module-flymake)
(remove-hook 'elpy-modules 'elpy-module-company)
(remove-hook 'elpy-modules 'elpy-module-yasnippet)
(remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

(elpy-enable)

(provide 'python)
;;; python.el ends here
