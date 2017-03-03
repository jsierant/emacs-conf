;;; package --- C++ editing support for Emacs
;;; Commentary:

;;; Code:

(require 'rtags)
(require 'company-rtags)
(require 'flycheck-rtags)
(require 'rtags-helm)
(require 'disaster)
(load "/usr/share/clang/clang-format.el")

(defun cpp/mode-setup()
  "Setup function for C++ mode"
   (set (make-local-variable 'company-backends)
        '((company-rtags
           :with company-yasnippet
           :with company-files
           )))

  (highlight-indent-guides-mode)

  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)

;  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;  (setq-local flycheck-check-syntax-automatically nil)

  (flycheck-select-checker 'rtags)
  (rtags-diagnostics 1)
  (company-mode)
  (flycheck-mode)
  (setq rtags-use-helm t)
  (flyspell-prog-mode)
)

(add-hook 'c-mode-hook 'cpp/mode-setup)
(add-hook 'c++-mode-hook 'cpp/mode-setup)
(add-hook 'objc-mode-hook 'cpp/mode-setup)


(evil-leader/set-key-for-mode
  'c++-mode
  "d" 'rtags-print-symbol-info
  "j" 'rtags-find-symbol-at-point
  "u" 'rtags-find-references-at-point
  "b" 'rtags-location-stack-back
  "f" 'rtags-location-stack-forward
  "r r" 'rtags-rename-symbol
  "r f" 'clang-format-region
  "r i" 'rtags-get-include-file-for-symbol
  "t p" 'rtags-preprocess-file
  "t a" 'disaster
  )

(evil-leader/set-key-for-mode
  'c-mode
  "d" 'rtags-print-symbol-info
  "j" 'rtags-find-symbol-at-point
  "u" 'rtags-find-references-at-point
  "b" 'rtags-location-stack-back
  "f" 'rtags-location-stack-forward
  "r r" 'rtags-rename-symbol
  "r f" 'clang-format-region
  "r i" 'rtags-get-include-file-for-symbol
  "t p" 'rtags-preprocess-file
  "t a" 'disaster
  )

(define-key c++-mode-map (kbd "<f5>") 'rtags-reparse-file)
(define-key c++-mode-map (kbd "<f4>") 'helm-flyspell-correct)
(define-key c-mode-map (kbd "<f5>") 'rtags-reparse-file)
(define-key c-mode-map (kbd "<f4>") 'helm-flyspell-correct)

(provide 'cpp)
;;; cpp.el ends here

