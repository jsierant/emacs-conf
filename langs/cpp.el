;;; package --- C++ editing support for Emacs
;;; Commentary:

;;; Code:

(require 'rtags)
(require 'company-rtags)
(require 'flycheck-rtags)
(require 'rtags-helm)
(require 'cmake-ide)

(defun cpp/mode-setup()
  "Setup function for C++ mode"
   (set (make-local-variable 'company-backends)
        '((company-rtags
           )))

  (highlight-indent-guides-mode)

  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)

  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil)

  (message "JS")
  (flycheck-select-checker 'rtags)
  (rtags-diagnostics 1)
  (company-mode 1)
  (flycheck-mode 1)
  (setq rtags-use-helm t)
)

(add-hook 'c-mode-hook 'cpp/mode-setup)
(add-hook 'c++-mode-hook 'cpp/mode-setup)
(add-hook 'objc-mode-hook 'cpp/mode-setup)


(evil-leader/set-key-for-mode
  'c++-mode
  "d s" 'rtags-print-symbol-info
  "g d" 'rtags-find-symbol-at-point
  "f r" 'rtags-find-references-at-point
  "g b" 'rtags-location-stack-back
  "g f" 'rtags-location-stack-forward
  "r s" 'rtags-rename-symbol
  "p p" 'rtags-preprocess-file
  "i f" 'rtags-get-include-file-for-symbol
  )

(define-key c++-mode-map (kbd "C-c d") 'rtags-print-symbol-info)
(define-key c++-mode-map (kbd "<f5>") 'rtags-reparse-file)


(provide 'cpp)
;;; cpp.el ends here

