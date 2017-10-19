;;; package --- C++ editing support for Emacs
;;; Commentary:

;;; Code:

(require 'lsp-mode)
(lsp-define-stdio-client
 'c++-mode "cpp" 'stdio
 #'(lambda () default-directory)
 "Clang Language Server"
 '("clangd") )

(use-package disaster)

(defun cpp/mode-setup()
  "Setup function for C++ mode"

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

(add-hook 'c-mode-hook 'cpp/mode-setup)
(add-hook 'c++-mode-hook 'cpp/mode-setup)
(add-hook 'objc-mode-hook 'cpp/mode-setup)


(evil-leader/set-key-for-mode
  'c++-mode
  "ta" 'disaster
  )

(evil-leader/set-key-for-mode
  'c-mode
  "ta" 'disaster
  )

(provide 'cpp)
;;; cpp.el ends here

