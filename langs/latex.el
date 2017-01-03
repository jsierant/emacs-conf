;;; package --- Latex editing support for Emacs

;;; Commentary:

;;; Code:

(require 'tex-mik)
(require 'tex)
(require 'company)
(require 'company-auctex)
(require 'flyspell)

(defun latex/mode-setup ()
  "Setups LaTeX mode"
   (set (make-local-variable 'company-backends)
        '((company-auctex
           :with company-yasnippet
           :with company-dabbrev
           )))
  (flyspell-mode)
  (LaTeX-math-mode)
  (turn-on-reftex)

  (company-mode)
  (company-quickhelp-mode 1)

  (eldoc-mode)
  )

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)


(add-hook 'LaTeX-mode-hook 'latex/mode-setup)

(provide 'latex)
;;; latex.el ends here
