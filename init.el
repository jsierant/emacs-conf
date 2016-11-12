;; view
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq x-super-keysym 'meta)

;; disable welcome screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
(setq tab-width 3)

;; line wrap disabled
(setq-default truncate-lines 1)

(set-frame-font "LiberationMono-14")



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
 'fill-column-indicator
 'highlight-indent-guides
 )

;; activate installed packages
(package-initialize)

(require 'darktooth-theme)


(require 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-leader ",")

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

;; column highlighting
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "darkgray")
(setq fci-rule-column 120)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(set-face-foreground 'highlight-indent-guides-character-face "darkgray")
(require 'whitespace)
(setq-default whitespace-style '(face trailing))
(global-whitespace-mode 1)

;; compilation
(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output t)
;; No prompt for command
(setq compilation-read-command nil)
(global-set-key (kbd "<f8>") 'compile)

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
(projectile-global-mode)
(require 'helm-projectile)
(helm-projectile-on)

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
(yas-global-mode 1)
(yas-reload-all)


;; git
(require 'magit)

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
(global-company-mode 1)
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

(require 'company-quickhelp)
(company-quickhelp-mode 1)

(require 'company-files)
(define-key global-map (kbd "C-c f") 'company-files)

(require 'company-elisp)

(require 'company-shell)


(define-key company-active-map (kbd "C-e") #'company-other-backend)


(require 'flycheck)

;; == languages

;; python
(require 'anaconda-mode)
(require 'company-anaconda)
(require 'flycheck-pyflakes)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(defun python/mode-setup()
  "Setup function for python mode"
  (setq-local company-backends
              '(company-anaconda
                company-yasnippet
                company-dabbrev
                company-quickhelp))
  
  (local-set-key (kbd "C-c d") 'anaconda-mode-show-doc)
  (flycheck-mode)
  (flycheck-select-checker 'python-flake8)
  (highlight-indent-guides-mode)
  (hc-highlight-tabs)
 )

(add-hook 'python-mode-hook 'python/mode-setup)

(evil-leader/set-key-for-mode
  'python-mode
  "s d" 'anaconda-mode-show-doc
  "g d" 'anaconda-mode-find-definitions
  "f r" 'anaconda-mode-find-references
  "g b" 'anaconda-mode-go-back)

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
  )

