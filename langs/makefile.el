;;; package --- Makefile editing support for Emacs
;;; Commentary:

;;; Code:

(require 'company)

(add-to-list 'auto-mode-alist '("Makefile\..+" . makefile-gmake-mode))

(load "~/.emacs.d/ltags.el")

(defun makefile/mode-setup()
  "Setup function for python mode"
   (set (make-local-variable 'company-backends)
        '((company-etags
           :with company-yasnippet
           :with company-files
           :with company-dabbrev
           )))

  (local-set-key (kbd "<f5>") 'ltags-update)
  (company-mode 1)
  (ltags-setup "Make")
  (setq-local makefile-warn-suspicious-lines nil)
)

(add-hook 'makefile-gmake-mode-hook 'makefile/mode-setup)

(evil-leader/set-key-for-mode
  'makefile-gmake-mode
  "g d" 'xref-find-definitions
  "g w d" 'xref-find-definitions-other-window
  "f r" 'xref-find-references
  "g b" 'xref-pop-marker-stack)


(provide 'makefile)
;;; makefile.el ends here
