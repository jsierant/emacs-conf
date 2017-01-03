;;; package --- Shell scritps editing support for Emacs
;;; Commentary:

;;; Code:

(require 'company-shell)

(require 'shelldoc)

(defun shell/mode-setup()
  "Setup function for python mode"
   (set (make-local-variable 'company-backends)
        '((company-shell
           :with company-yasnippet
           :with company-files
           :with company-dabbrev
           )))

  (highlight-indent-guides-mode)
  (local-set-key (kbd "C-c d") 'describe-symbol)

  (company-mode)
  (flycheck-mode)
  (shelldoc-minor-mode-on)
)

(add-hook 'sh-mode-hook 'shell/mode-setup)

(evil-leader/set-key-for-mode
  'sh-mode
  "s d" nil
  "g d" 'anaconda-mode-find-definitions
  "f r" 'anaconda-mode-find-references
  "g b" 'pop-tag-mark)


(provide 'shell)
;;; shell.el ends here

