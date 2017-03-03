;;; package --- Lua editing support for EMACS

;;; Commentary:

;;; Code:


(require 'company-quickhelp)
(require 'company-lua)


(defun lua/mode-setup()
  "Setup function for lua mode"
   (set (make-local-variable 'company-backends)
        '((company-lua
           :with company-yasnippet
           :with company-files
           )))

  (company-mode)
  (company-quickhelp-mode 1)
  (local-set-key (kbd "<f5>") 'ltags-update)
  (ltags-setup "Lua")
)

(add-hook 'lua-mode-hook 'lua/mode-setup)

(evil-leader/set-key-for-mode
  'lua-mode
  "j" 'xref-find-definitions
  "w j" 'xref-find-definitions-other-window
  "u" 'xref-find-references
  "b" 'xref-pop-marker-stack
  )

(provide 'lua)
;;; lua.el ends here
