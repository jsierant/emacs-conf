;;; package --- Python editing support for EMACS

;;; Commentary:

;;; Code:

(defun python/mode-setup()
  "Setup function for python mode"

  (highlight-indent-guides-mode)
  (lsp-mode)
  (flycheck-mode)
  (set (make-local-variable 'company-backends)
       '((company-capf :separate company-dabbrev-code company-yasnippet)))
  (setq company-auto-complete-chars "\.")
  (company-mode)
  (yas-minor-mode)
  (flyspell-prog-mode)
)

(add-hook 'python-mode-hook 'python/mode-setup)

;; (provide 'python)
;;; python.el ends here
