;;; package -- Emacs configuration

;;; Commentary:


;;; Code:
;; speedup setup time
(setq gc-cons-threshold most-positive-fixnum)
(add-hook
 'minibuffer-exit-hook
 (lambda ()
   (setq gc-cons-threshold 800000)
   )
 )

(defun modeline-set-lighter (minor-mode lighter)
  (when (assq minor-mode minor-mode-alist)
    (setcar (cdr (assq minor-mode minor-mode-alist)) lighter)))

(defun modeline-remove-lighter (minor-mode)
 (modeline-set-lighter minor-mode ""))


;; view
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq x-super-keysym 'meta)
(setq x-alt-keysym 'alt)
;; (setq frame-resize-pixelwise t)

;; disable welcome screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
(setq tab-width 3)

;; line wrap disabled
(setq-default truncate-lines 1)

(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?- "w" (standard-syntax-table))

(setq ring-bell-function 'ignore)

;; Makes *scratch* empty.
(setq initial-scratch-message "")

(defun kill-buffer-if-exist (buffer)
  (if(get-buffer buffer)
      (kill-buffer buffer)))

;; Removes *scratch* from buffer after the mode has been set.
;; (add-hook 'after-change-major-mode-hook
;;           (lambda () (kill-buffer-if-exist "*scratch*")))

;; Removes *messages* from the buffer.
;; (setq-default message-log-max nil)
;; (kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
          (lambda () (kill-buffer-if-exist "*Completions*")))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; packaging
(require 'package)
(setq package-user-dir "~/.emacs.d/site-lisp")
(setq load-prefer-newer t)
(setq package-archives
      '(
        ;; ("gnu" . "https://elpa.gnu.org/packages/")
        ;; ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))
      )
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
;;(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; theme initialization
(defvar margin-background-color "#1c1c1c")
(defvar gitgutter-background-color "#282828")
(defun init-theme-impl ()
  "Init theme."
 (set-frame-font "LiberationMono-9")
 (load-theme 'darktooth t)
 (set-face-attribute 'fringe nil :background margin-background-color)
)

(defun init-theme ()
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                '(lambda (f)
                   (with-selected-frame f
                     (when (window-system f)
                       (init-theme-impl)))))
  (init-theme-impl)))

(use-package darktooth-theme
  :config (init-theme))

(use-package powerline
  :config
 (setq powerline-utf-8-separator-left        #xe0b0
       powerline-utf-8-separator-right       #xe0b2
       airline-utf-glyph-separator-left      #xe0b0
       airline-utf-glyph-separator-right     #xe0b2
       airline-utf-glyph-subseparator-left   #xe0b1
       airline-utf-glyph-subseparator-right  #xe0b3
       airline-utf-glyph-branch              #xe0a0
       airline-utf-glyph-readonly            #xe0a2
       airline-utf-glyph-linenumber          #xe0a1)
 )

(use-package airline-themes
  :config (load-theme 'airline-cool t))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "c" 'comment-dwim
    "fr" 'xref-find-references
    "j"  'xref-find-definition
    "ff" 'helm-find-files
    "fw" 'helm-do-grep-ag)
    "b" (quote evil-jump-backward)
    "f" (quote evil-jump-forward))


(use-package evil-noautochdir
  :init
  (add-to-list 'load-path "~/.emacs.d/site-lisp/evil-noautochdir")
  :ensure nil)

(use-package evil
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map "L" 'evil-window-right)
  (define-key evil-normal-state-map "H" 'evil-window-left)
  (define-key evil-normal-state-map "K" 'evil-window-up)
  (define-key evil-normal-state-map "J" 'evil-window-down)
  (define-key evil-normal-state-map (kbd "SPC") 'helm-buffers-list)
  (define-key evil-motion-state-map "L" 'evil-window-right)
  (define-key evil-motion-state-map "H" 'evil-window-left)
  (define-key evil-motion-state-map "K" 'evil-window-up)
  (define-key evil-motion-state-map "J" 'evil-window-down)
  (define-key evil-motion-state-map (kbd "SPC") 'helm-buffers-list))

(use-package auto-compile
  :config
  (auto-compile-on-save-mode))

(defun compile-self ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

(use-package git-gutter
  :config
  (setq git-gutter-sign "▌")
  (set-face-background 'git-gutter:modified gitgutter-background-color)
  (set-face-background 'git-gutter:added gitgutter-background-color)
  (set-face-background 'git-gutter:deleted gitgutter-background-color)
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  (custom-set-variables
   '(git-gutter:added-sign git-gutter-sign)
   '(git-gutter:deleted-sign git-gutter-sign)
   '(git-gutter:modified-sign git-gutter-sign))
  (modeline-remove-lighter 'git-gutter-mode)
  )


(use-package helm
   :config
   (global-set-key (kbd "C-c h") 'helm-command-prefix)
   (global-unset-key (kbd "C-x c"))
   (global-set-key (kbd "M-x") 'helm-M-x)
   (global-set-key (kbd "C-x b") 'helm-mini)
   (setq helm-grep-ag-command "ag --line-numbers -S --hidden --color --color-match '31;43' --nogroup %s %s %s")
   (setq helm-grep-ag-pipe-cmd-switches '("--color-match '31;43'"))
   (setq helm-mode-fuzzy-match t)
   (setq helm-split-window-in-side-p nil)
   (setq helm-move-to-line-cycle-in-source t)
   (setq helm-scroll-amount 10)
   (helm-autoresize-mode 1)
   (setq helm-autoresize-max-height 20)
   (helm-mode)
   (defadvice helm-display-mode-line (after undisplay-header activate)
     (setq header-line-format nil) )
   (modeline-remove-lighter 'helm-mode) )

(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  (defun projectile-frame-title-format ()
    "Return frame title with current project name, where applicable."
    (let ((file buffer-file-name))
      (if file
          (concat (when (and (bound-and-true-p projectile-mode)
                             (projectile-project-p))
                    (format " [%s]" (projectile-project-name)))
                  " "
                  (file-name-nondirectory file))
        "%b")))

  (when (display-graphic-p)
    (setq frame-title-format '((:eval (projectile-frame-title-format)))))
  )

(use-package helm-projectile
  :config
  (helm-projectile-on))


(modeline-remove-lighter 'undo-tree-mode)

(use-package graphviz-dot-mode)

;; programming

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (set-face-foreground 'highlight-indent-guides-character-face "darkgray"))
  

(use-package flycheck
  :init
  (defun flycheck-error-format-extension(err)
    (let ((checker (symbol-name (flycheck-error-checker err)))
          (level (symbol-name (flycheck-error-level err))))
      (format "(%s:%s)" checker level)))
  (defun flycheck-error-format-extended(orig-fun &rest err)
    (let ((orig-message (apply orig-fun err))
          (extension (apply #'flycheck-error-format-extension err)))
      (format "%s %s" orig-message extension)))

  (advice-add 'flycheck-error-format-message-and-id
              :around #'flycheck-error-format-extended)
  (use-package flycheck-popup-tip
    :init
    (add-to-list 'load-path "~/.emacs.d/site-lisp/flycheck-popup-tip")
    :config
    (setq flycheck-popup-tip-error-prefix "► ")
    )
  :config
  (setq flycheck-display-errors-delay 0.5)
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
  (modeline-remove-lighter 'flycheck-mode)

  (flycheck-define-checker pycodestyle
    "Python codestyle checker"
    :command ("pycodestyle" "-")
    :standard-input t
    :error-patterns
    ((warning line-start
                "stdin:" line ":" (optional column ":") " "
                (id (one-or-more (any alpha)) (one-or-more digit)) " "
                (message (one-or-more not-newline))
                line-end))
    :next-checkers (python-pylint)
    :modes python-mode))


(add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-mode")

(require 'lsp-mode)
(require 'lsp-flycheck)
(use-package lsp-python)
(setq lsp-enable-eldoc nil)

(modeline-remove-lighter 'eldoc-mode)

(use-package company
  :config
  (use-package yasnippet
    :config
    (setq yas-snippet-dirs
          '("~/.emacs.d/snippets"))
    (yas-reload-all)
    (require 'company-yasnippet) )

  (setq company-auto-complete t)
  (global-set-key (kbd "C-SPC") 'company-complete)
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 3)
  (modeline-remove-lighter 'company-mode)
  (define-key company-active-map [tab] 'company-select-next)
  (define-key company-active-map [backtab] 'company-select-previous)
  (setq company-frontends
        '(company-echo-frontend
          company-pseudo-tooltip-frontend
          ;; company-quickhelp-frontend
          ))
  (require 'company-capf)
  (require 'company-abbrev)
  (require 'company-elisp)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-other-buffers t) )

(load "~/.emacs.d/langs/python.el")
(load "~/.emacs.d/langs/cpp.el")
(load "~/.emacs.d/langs/elisp.el")


(load-file "~/.emacs.d/buildsystem/buildsystem.el")

(use-package neotree
  :config
  (setq neo-smart-open t)
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map [f5] 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd ".") 'neotree-hidden-file-toggle)
              (define-key evil-normal-state-local-map (kbd "l") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "r") 'neotree-rename-node)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
              (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
  (global-set-key [f2] 'neotree-toggle))

;; (add-hook 'compilation-mode-hook 'my-compilation-hook)
;; (ensure-package-installed
;;  'darktooth-theme
;;  'powerline
;;  'airline-themes
;;  'evil
;;  'evil-matchit
;;  'evil-leader
;;  'helm
;;  'projectile
;;  'helm-projectile
;;  'yasnippet
;;  'magit
;;  'git-gutter
;;  'company
;;  'company-quickhelp
;;  'company-shell
;;  'flycheck
;;  'flycheck-pyflakes
;;  'highlight-indent-guides
;;  'elisp-slime-nav
;;  'shelldoc
;;  'helm-make
;;  'cmake-mode
;;  'cmake-font-lock
;;  'rtags
;;  'flycheck-clang-tidy
;;  'flyspell
;;  'helm-flyspell
;;  'disaster
;;  'direx
;;  'color-identifiers-mode
;;  'rainbow-delimiters
;;  'highlight-symbol
;;  'elpy
;;  'company-jedi
;;  'auctex
;;  'company-auctex
;;  'popwin
;;  'linum-relative
;;  'lua-mode
;;  'company-lua
;;  )

;; (add-to-list 'load-path "~/.emacs.d/modules/markdown-mode")



;; (require 'highlight-indent-guides)
;; (setq highlight-indent-guides-method 'character)
;; (set-face-foreground 'highlight-indent-guides-character-face "darkgray")

;; (require 'whitespace)
;; (setq-default whitespace-style '(face trailing spaces tabs))
;; (add-hook 'prog-mode-hook 'whitespace-mode)

;; (setq make-backup-files nil)

;; (require 'color-identifiers-mode)
;; (global-color-identifiers-mode)
;; (require 'rainbow-delimiters)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; (require 'highlight-symbol)

;; (require 'evil-leader)
;; (global-evil-leader-mode)

;; (evil-leader/set-leader ",")

;; (load "~/.emacs.d/evil-noautochdir/evil-noautochdir.el")

;; (require 'evil)
;; (evil-mode 1)

;; (require 'powerline)

;; (require 'airline-themes)
;; (load-theme 'airline-cool t)

;; (setq powerline-utf-8-separator-left        #xe0b0
;;       powerline-utf-8-separator-right       #xe0b2
;;       airline-utf-glyph-separator-left      #xe0b0
;;       airline-utf-glyph-separator-right     #xe0b2
;;       airline-utf-glyph-subseparator-left   #xe0b1
;;       airline-utf-glyph-subseparator-right  #xe0b3
;;       airline-utf-glyph-branch              #xe0a0
;;       airline-utf-glyph-readonly            #xe0a2
;;       airline-utf-glyph-linenumber          #xe0a1)

;; ;; compilation
;; (require 'helm-make)
;; (global-set-key (kbd "<f8>") 'projectile-compile-project)
;; (setq helm-make-list-target-method "qp")
;; (global-set-key (kbd "<f7>") 'helm-make-projectile)

;; ;; project grep/find files
;; (global-set-key (kbd "C-M-f") 'helm-projectile-find-file)
;; (global-set-key (kbd "C-M-g") 'helm-projectile-grep)

;; ;; close window on successfull build
;; (setq compilation-finish-functions
;;   (lambda (buf str)
;;     (if (null (string-match ".*exited abnormally.*" str))
;;         ;;no errors, make the compilation window go away in a few seconds
;;         (progn
;;           (run-at-time
;;            "4 sec" nil 'delete-windows-on
;;            (get-buffer-create "*compilation*"))
;;           (message "No Compilation Errors!")))))
;; (setq compilation-window-height 15)
;; (require 'popwin)
;; (popwin-mode 1)

;; ;;Press “%” to jump between matched tags in Emacs. For example, in HTML “<div>” and “</div>” are a pair of tags.
;; (require 'evil-matchit)
;; (global-evil-matchit-mode 1)



;; ;; git
;; (require 'magit)
;; (global-set-key (kbd "C-x g s") 'magit-status)
;; (global-set-key (kbd "C-x g c i") 'magit-commit)

;; ;; (require 'git-gutter+)


;; ;; (setq git-gutter+-modified-sign "│")
;; ;; (setq git-gutter+-added-sign "│")
;; ;; (setq git-gutter+-deleted-sign "│")

;; ;; (add-hook 'prog-mode-hook 'git-gutter+-mode)
;; ;; (setq git-gutter+-hide-gutter t)



;; ;; completion
;; (require 'company)
;; ;(setq company-auto-complete nil)

;; (setq company-idle-delay 0.1)
;; (setq company-minimum-prefix-length 2)
;; (setq company-transformers '(company-sort-by-backend-importance))


;; (require 'company-dabbrev)
;; (setq company-dabbrev-downcase nil)
;; (setq company-dabbrev-other-buffers t)


;; (global-set-key (kbd "C-SPC") nil)

;; (global-set-key (kbd "C-c w") 'company-dabbrev)
;; (global-set-key (kbd "C-SPC") 'company-complete)

;; (require 'company-yasnippet)
;; (global-set-key (kbd "C-c s") 'company-yasnippet)

;; (require 'company-files)
;; (define-key global-map (kbd "C-c f") 'company-files)

;; (define-key company-active-map (kbd "C-j") #'company-select-next)
;; (define-key company-active-map (kbd "C-k") #'company-select-previous)

;; (setq company-frontends
;;       '(company-echo-frontend
;;         company-pseudo-tooltip-frontend
;;         company-quickhelp-frontend
;;         )
;;       )

;; (require 'flycheck)
;; (require 'flyspell)
;; (setq flyspell-issue-welcome-flag nil)
;; (require 'helm-flyspell)

;; (require 'rtags)

;; ;; == languages
;; (load "~/.emacs.d/langs/python.el")
;; (load "~/.emacs.d/langs/elisp.el")
;; (load "~/.emacs.d/langs/shell.el")
;; (load "~/.emacs.d/langs/makefile.el")
;; (load "~/.emacs.d/langs/cmake.el")
;; (load "~/.emacs.d/langs/cpp.el")
;; (load "~/.emacs.d/langs/latex.el")
;; (load "~/.emacs.d/langs/markdown.el")
;; (load "~/.emacs.d/langs/lua.el")


;; (load "~/.emacs.d/cmake-project.el")

;; ;; window split and navigation
;; (defadvice split-window (after move-point-to-new-window activate)
;;   "Moves the point to the newly created window after splitting."
;;   (other-window 1))

;; (evil-leader/set-key
;;   "w r" 'evil-window-right
;;   "w l" 'evil-window-left
;;   "w b" 'evil-window-down
;;   "w a" 'evil-window-up
;;   "h" 'highlight-symbol
;;   "c" 'comment-dwim
;;   )

;; (define-key evil-normal-state-map " " 'helm-mini)
;; (helm-mode)

;; ;; Makes *scratch* empty.
;; (setq initial-scratch-message "")

;; ;; Removes *scratch* from buffer after the mode has been set.
;; (defun remove-scratch-buffer ()
;;   (if (get-buffer "*scratch*")
;;       (kill-buffer "*scratch*")))
;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; ;; Removes *messages* from the buffer.
;; ;(setq-default message-log-max nil)
;; ;(kill-buffer "*Messages*")

;; ;; Removes *Completions* from buffer after you've opened a file.
;; (add-hook 'minibuffer-exit-hook
;;       '(lambda ()
;;          (let ((buffer "*Completions*"))
;;            (and (get-buffer buffer)
;;                 (kill-buffer buffer)))))

;; ;; Don't show *Buffer list* when opening multiple files at the same time.
;; (setq inhibit-startup-buffer-menu t)

;; (defun projectile-frame-title-format ()
;;     "Return frame title with current project name, where applicable."
;;     (let ((file buffer-file-name))
;;        (if file
;;            (concat (when (and (bound-and-true-p projectile-mode)
;;                               (projectile-project-p))
;;                      (format " [%s]" (projectile-project-name)))
;;                    " "
;;                    (file-name-nondirectory file))
;;            "%b")))

;;   (when (display-graphic-p)
;;     (setq frame-title-format '((:eval (projectile-frame-title-format)))))


;; (setq debug-on-error t)
;; (setq enable-local-eval t)

;; (require 'direx)
;; (push '(direx:direx-mode :position left :width 25 :dedicated t)
;;       popwin:special-display-config)
;; (global-set-key (kbd "<f6>") 'direx:jump-to-directory-other-window)

;; (evil-set-initial-state 'direx:direx-mode 'emacs)
;; (define-key direx:direx-mode-map (kbd "j") 'direx:next-item)
;; (define-key direx:direx-mode-map (kbd "k") 'direx:previous-item)


;; (global-unset-key (kbd "S-k"))
;; (global-unset-key (kbd "S-j"))

;; (global-auto-revert-mode nil)

;; (defun modeline-set-lighter (minor-mode lighter)
;;   (when (assq minor-mode minor-mode-alist)
;;     (setcar (cdr (assq minor-mode minor-mode-alist)) lighter)))

;; (defun modeline-remove-lighter (minor-mode)
;;   (modeline-set-lighter minor-mode ""))


;; (modeline-remove-lighter 'helm-mode)
;; (modeline-remove-lighter 'git-gutter-mode)
;; (modeline-remove-lighter 'company-mode)
;; (modeline-remove-lighter 'color-identifiers-mode)
;; (modeline-remove-lighter 'undo-tree-mode)
;; (modeline-remove-lighter 'elpy-mode)
;; (modeline-remove-lighter 'flycheck-mode)
;; (modeline-remove-lighter 'flyspell-mode)
;; (modeline-remove-lighter 'auto-revert-mode)
;; (modeline-remove-lighter 'yas-minor-mode)
;; (modeline-remove-lighter 'whitespace-mode)
;; (modeline-remove-lighter 'elisp-slime-nav-mode)
;; (modeline-remove-lighter 'abbrev-mode)
;; (modeline-remove-lighter 'linum-relative-mode)

(defun package--save-selected-packages (&optional value)
  "Set and save `package-selected-packages' to VALUE."
  (when value
    (setq package-selected-packages value)))

(provide 'init)
;;; init.el ends here
