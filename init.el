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

;; view
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq x-super-keysym 'meta)
(setq x-alt-keysym 'alt)

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

;; packaging
(require 'package)
(setq package-user-dir "~/.emacs.d/site-lisp")
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
;;(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; theme initialization
(defun init-theme-impl ()
  "Init theme."
 (set-frame-font "LiberationMono-9")
 (load-theme 'darktooth t)
;; (set-face-attribute 'fringe nil :background margin-background-color)
;; (set-face-foreground 'git-gutter:modified "purple")
;; (set-face-foreground 'git-gutter:added "green")
;; (set-face-foreground 'git-gutter:deleted "red")
)

(defun init-theme ()
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                '(lambda (f)
                   (with-selected-frame f
                     (when (window-system f)
                       (init-theme-impl)))))
  (init-theme-impl))
)

(use-package darktooth-theme
  :config (init-theme))


;; ;; packages
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/") t)
;; (package-initialize)

;; (defun ensure-package-installed (&rest packages)
;;   "Assure all PACKAGES are installed, ask for installation if it’s not.

;; Return a list of installed packages or nil for every skipped package."
;;   (mapcar
;;    (lambda (package)
;;      ;; (package-installed-p 'evil)
;;      (if (package-installed-p package)
;;          nil
;;        (if (y-or-n-p (format "Package %s is missing. Install it? " package))
;;            (package-install package)
;;          package)))
;;    packages))

;; ;; make sure to have downloaded archive description.
;; ;; Or use package-archive-contents as suggested by Nicolas Dudebout
;; (or (file-exists-p package-user-dir)
;;     (package-refresh-contents))

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

;; ;; activate installed packages
;; (package-initialize)

;; (defvar margin-background-color "#1c1c1c")

;; (require 'git-gutter)
;; (git-gutter:linum-setup)
;; (add-hook 'prog-mode-hook 'git-gutter-mode)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(git-gutter:added-sign "│")
;;  '(git-gutter:deleted-sign "│")
;;  '(git-gutter:modified-sign "│")
;;  )

;; (require 'darktooth-theme)
;; (defun inittheme ()
;;   "Inits theme."
;;  (set-frame-font "LiberationMono-10")
;;  (load-theme 'darktooth t)
;;  (set-face-attribute 'fringe nil :background margin-background-color)
;;  (set-face-foreground 'git-gutter:modified "purple")
;;  (set-face-foreground 'git-gutter:added "green")
;;  (set-face-foreground 'git-gutter:deleted "red")
;;  )


;; (if (daemonp)
;; (add-hook 'after-make-frame-functions
;;           '(lambda (f)
;;              (with-selected-frame f
;;                (when (window-system f)
;;                  (inittheme)))))
;;  (inittheme))


;; ;; Line numbers
;; (require 'linum-relative)
;; (add-hook 'prog-mode-hook 'linum-relative-mode)
;; (add-hook 'text-mode-hook 'linum-relative-mode)
;; (setq linum-relative-current-symbol "")

;; (defun linum-relative-setup-evil ()
;;   "Setup nlinum-relative-mode for evil."
;;   (interactive)
;;   (add-hook 'evil-insert-state-entry-hook
;;             (lambda () (when (bound-and-true-p linum-relative-mode) (linum-relative-off))))
;;   (add-hook 'evil-insert-state-exit-hook
;;             (lambda () (when (bound-and-true-p linum-relative-mode) (linum-relative-on))))
;;   (add-hook 'evil-normal-state-entry-hook
;;             (lambda () (when (bound-and-true-p linum-relative-mode) (linum-relative-on))))
;;   (add-hook 'evil-normal-state-exit-hook
;;             (lambda () (when (bound-and-true-p linum-relative-mode) (linum-relative-off))))
;;   (add-hook 'evil-visual-state-entry-hook
;;             (lambda () (when (bound-and-true-p linum-relative-mode) (linum-relative-on))))
;;   (add-hook 'evil-visual-state-exit-hook
;;             (lambda () (when (bound-and-true-p linum-relative-mode) (linum-relative-off))))
;;   )

;; (linum-relative-setup-evil)


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


;; (require 'helm)
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (evil-leader/set-key
;;   "l b" 'helm-buffers-list)

;; (setq
;;  helm-split-window-in-side-p           t
;;    ; open helm buffer inside current window,
;;    ; not occupy whole other window
;;  helm-move-to-line-cycle-in-source     t
;;    ; move to end or beginning of source when
;;    ; reaching top or bottom of source.
;;  helm-ff-search-library-in-sexp        t
;;    ; search for library in `require' and `declare-function' sexp.
;;  helm-scroll-amount                    8
;;    ; scroll 8 lines other window using M-<next>/M-<prior>
;;  helm-ff-file-name-history-use-recentf t
;;  ;; Allow fuzzy matches in helm semantic
;;  helm-semantic-fuzzy-match t
;;  helm-imenu-fuzzy-match    t)
;; ;; Have helm automaticaly resize the window
;; (helm-autoresize-mode 1)


;; (require 'projectile)
;; (projectile-mode 1)
;; (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))

;; (require 'helm-projectile)
;; (helm-projectile-on)

;; (require 'yasnippet)
;; (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
;; (yas-global-mode 1)
;; (yas-reload-all)

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


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "│")
 '(git-gutter:deleted-sign "│")
 '(git-gutter:modified-sign "│")
 '(package-selected-packages
   (quote
    (shelldoc rtags rainbow-delimiters popwin nlinum-relative neotree magit linum-relative highlight-symbol highlight-indent-guides helm-projectile helm-make helm-flyspell git-gutter-fringe+ git-gutter flycheck-pyflakes flycheck-clang-tidy evil-matchit evil-leader esup elpy elisp-slime-nav disaster direx darktooth-theme company-shell company-quickhelp company-lua company-jedi company-auctex column-marker color-identifiers-mode cmake-font-lock autopair airline-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
