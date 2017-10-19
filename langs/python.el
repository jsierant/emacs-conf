;;; package --- Python editing support for EMACS

;;; Commentary:

;;; Code:

(defun python/mode-setup()
  "Setup function for python mode"

  (highlight-indent-guides-mode)
  (lsp-mode)
  (flycheck-mode)

)

(add-hook 'python-mode-hook 'python/mode-setup)

;; (provide 'python)
;;; python.el ends here
