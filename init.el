;;; package -- Emacs configuration

;;; Commentary:


;;; Code:

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

(set-frame-font "LiberationMono-11")

(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?- "w" (standard-syntax-table))

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed
 'darktooth-theme
 'powerline
 'airline-themes
 'evil
 'evil-matchit
 'evil-leader
 'helm
 'projectile
 'helm-projectile
 'yasnippet
 'magit
 'git-gutter
 'company
 'company-quickhelp
 'company-shell
 'company-anaconda
 'flycheck
 'flycheck-pyflakes
 'highlight-indent-guides
 'elisp-slime-nav
 'shelldoc
 'helm-make
 'cmake-mode
 'cmake-font-lock
 'rtags
 'flyspell
 'helm-flyspell
 'disaster
 'neotree
 'autopair
 'color-identifiers-mode
 'rainbow-delimiters
 'highlight-symbol
 )

;; activate installed packages
(package-initialize)

(require 'darktooth-theme)

(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(set-face-foreground 'highlight-indent-guides-character-face "darkgray")

(require 'whitespace)
(setq-default whitespace-line-column 120)
(setq-default whitespace-style '(face lines-tail trailing spaces tabs))
(add-hook 'prog-mode-hook 'whitespace-mode)

(require 'autopair)
(autopair-global-mode)

(setq make-backup-files nil)

(require 'color-identifiers-mode)
(global-color-identifiers-mode)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'highlight-symbol)

(require 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-leader ",")

(load "~/.emacs.d/evil-noautochdir/evil-noautochdir.el")

(require 'evil)
(evil-mode 1)

(require 'powerline)

(require 'airline-themes)
(load-theme 'airline-cool t)

(setq powerline-utf-8-separator-left        #xe0b0
      powerline-utf-8-separator-right       #xe0b2
      airline-utf-glyph-separator-left      #xe0b0
      airline-utf-glyph-separator-right     #xe0b2
      airline-utf-glyph-subseparator-left   #xe0b1
      airline-utf-glyph-subseparator-right  #xe0b3
      airline-utf-glyph-branch              #xe0a0
      airline-utf-glyph-readonly            #xe0a2
      airline-utf-glyph-linenumber          #xe0a1)

;; compilation
(require 'helm-make)
(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output t)
;; No prompt for command
(setq compilation-read-command nil)
(global-set-key (kbd "<f8>") 'projectile-compile-project)
(setq helm-make-list-target-method "qp")
(global-set-key (kbd "<f7>") 'helm-make-projectile)

;; project grep/find files
(global-set-key (kbd "C-M-f") 'helm-projectile-find-file)
(global-set-key (kbd "C-M-g") 'helm-projectile-grep)

;; close window on successfull build
(setq compilation-finish-functions
  (lambda (buf str)
    (if (null (string-match ".*exited abnormally.*" str))
        ;;no errors, make the compilation window go away in a few seconds
        (progn
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!")))))
(setq compilation-window-height 15)

;; line numbers - only on go to line
;;(global-linum-mode 1)
(require 'linum)
(setq linum-format " %3d\u2502")
(global-set-key (kbd "C-g") 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)   (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
      (linum-mode -1)))


;;Press “%” to jump between matched tags in Emacs. For example, in HTML “<div>” and “</div>” are a pair of tags.
(require 'evil-matchit)
(global-evil-matchit-mode 1)


(require 'helm)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(evil-leader/set-key
  "l b" 'helm-buffers-list)

(setq
 helm-split-window-in-side-p           t
   ; open helm buffer inside current window,
   ; not occupy whole other window
 helm-move-to-line-cycle-in-source     t
   ; move to end or beginning of source when
   ; reaching top or bottom of source.
 helm-ff-search-library-in-sexp        t
   ; search for library in `require' and `declare-function' sexp.
 helm-scroll-amount                    8
   ; scroll 8 lines other window using M-<next>/M-<prior>
 helm-ff-file-name-history-use-recentf t
 ;; Allow fuzzy matches in helm semantic
 helm-semantic-fuzzy-match t
 helm-imenu-fuzzy-match    t)
;; Have helm automaticaly resize the window
(helm-autoresize-mode 1)


(require 'projectile)
(projectile-mode 1)
(setq projectile-mode-line '(:eval (format " pro[%s]" (projectile-project-name))))

(require 'helm-projectile)
(helm-projectile-on)

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
(yas-global-mode 1)
(yas-reload-all)

;; git
(require 'magit)
1
(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g c i") 'magit-commit)

(require 'git-gutter)
(global-git-gutter-mode t)

(custom-set-variables
 '(git-gutter:added-sign "│")
 '(git-gutter:deleted-sign "│")
 '(git-gutter:modified-sign "│")
 '(git-gutter:update-interval 2)
 )
(add-hook 'after-save-hook 'git-gutter:update-all-windows)

(set-face-foreground 'git-gutter:modified "purple") ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")

;; completion
(require 'company)
;(setq company-auto-complete nil)

(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)


(require 'company-dabbrev)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-other-buffers t)


(global-set-key (kbd "C-SPC") nil)

(global-set-key (kbd "C-c w") 'company-dabbrev)
(global-set-key (kbd "C-SPC") 'company-complete)

(require 'company-yasnippet)
(global-set-key (kbd "C-c s") 'company-yasnippet)

(require 'company-files)
(define-key global-map (kbd "C-c f") 'company-files)

(define-key company-active-map (kbd "C-e") #'company-other-backend)


(require 'flycheck)
(require 'flyspell)
(setq flyspell-issue-welcome-flag nil)
(require 'helm-flyspell)

(require 'rtags)

;; == languages
(load "~/.emacs.d/langs/python.el")
(load "~/.emacs.d/langs/elisp.el")
(load "~/.emacs.d/langs/shell.el")
(load "~/.emacs.d/langs/makefile.el")
(load "~/.emacs.d/langs/cmake.el")
(load "~/.emacs.d/langs/cpp.el")

;; window split and navigation
(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

(evil-leader/set-key
  "s w r" 'split-window-right
  "s w l" 'split-window-left
  "s w b" 'split-window-below
  "s w a" 'split-window-above
  "f w r" 'evil-window-right
  "f w l" 'evil-window-left
  "f w b" 'evil-window-down
  "f w a" 'evil-window-up
  "h w" 'highlight-symbol
  )

(define-key evil-normal-state-map " " 'helm-mini)
(helm-mode)

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
;(setq-default message-log-max nil)
;(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

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


(setq debug-on-error t)
(setq enable-local-eval t)

(require 'neotree)
(defun neotree-project-dir ()
   "Open NeoTree using the git root."
   (interactive)
   (let ((project-dir (projectile-project-root))
         (file-name (buffer-file-name)))
     (if project-dir
         (if (neotree-toggle)
             (progn
               (neotree-dir project-dir)
               (neotree-find file-name)))
       (message "Could not find git project root."))))

(global-set-key (kbd "<f6>") 'neotree-project-dir)

(add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(load "~/.emacs.d/package-selected-packages.el")
(provide 'init)
;;; init.el ends here
