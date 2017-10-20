;;; package --- Elisp suppport for emacs

;;; Commentary:

;;; Code:


(use-package elisp-slime-nav
  :config
  (modeline-remove-lighter 'elisp-slime-nav-mode) )

(defun elisp/mode-setup()
  "Setup function for elisp mode"
   (set (make-local-variable 'company-backends)
        '((:seperate
           company-elisp
           company-yasnippet
           company-files
           company-dabbrev
           )))

  (flycheck-mode)

  (elisp-slime-nav-mode)
  (company-mode)
  ;; (company-quickhelp-mode 1)
  )

(add-hook 'ielm-mode-hook 'elisp/mode-setup)
(add-hook 'emacs-lisp-mode-hook 'elisp/mode-setup)
(add-hook 'lisp-mode-hook 'elisp/mode-setup)

(evil-leader/set-key-for-mode
  'emacs-lisp-mode
  "d" 'describe-symbol
  "j" 'elisp-slime-nav-find-elisp-thing-at-point
  "b" 'pop-tag-mark)

(evil-leader/set-key-for-mode
  'ielm
  "d" 'describe-symbol
  "j" 'elisp-slime-nav-find-elisp-thing-at-point
  "b" 'pop-tag-mark)


(provide 'elisp)
