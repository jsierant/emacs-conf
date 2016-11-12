

(require 'company-elisp)
(require 'elisp-slime-nav)

(defun elisp/mode-setup()
  "Setup function for elisp mode"
   (set (make-local-variable 'company-backends)
        '((company-elisp
           company-yasnippet
           company-files
           company-dabbrev
           )))

  (flycheck-mode)

  (elisp-slime-nav-mode)
  (local-set-key (kbd "C-c d") 'describe-symbol)
  (company-mode)
  )

(add-hook 'ielm-mode-hook 'elisp/mode-setup)
(add-hook 'emacs-lisp-mode-hook 'elisp/mode-setup)

(evil-leader/set-key-for-mode
  'emacs-lisp-mode
  "s d" 'describe-symbol
  "g d" 'elisp-slime-nav-find-elisp-thing-at-point
  "g b" 'pop-tag-mark)

(evil-leader/set-key-for-mode
  'ielm
  "s d" 'describe-symbol
  "g d" 'elisp-slime-nav-find-elisp-thing-at-point
  "g b" 'pop-tag-mark)
