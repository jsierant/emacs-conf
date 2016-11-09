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
 'helm
 'projectile
 'helm-projectile
 'yasnippet
 'magit
 'company
 'company-quickhelp
 )

;; activate installed packages
(package-initialize)

(require 'darktooth-theme)

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

(defun goto-line-with-feedback ()   "Show line numbers temporarily, while prompting for the line number input"   (interactive)   (unwind-protect
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
(require 'helm-projectile)
(helm-projectile-on)

(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets/")
(yas-global-mode 1)
(yas-reload-all)


;; git
(require 'magit)

(global-set-key (kbd "C-x g s") 'magit-status)

;; completion
(require 'company)
(global-company-mode 1)
(setq company-auto-complete nil)

(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 3)


(require 'company-dabbrev)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-other-buffers t)

(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC w") 'company-dabbrev)
(global-set-key (kbd "C-SPC c") 'company-complete)
(global-set-key (kbd "C-SPC a") 'company-complete-common)

(require 'company-yasnippet)
(global-set-key (kbd "C-SPC s") 'company-yasnippet)

(require 'company-quickhelp)
(company-quickhelp-mode 1)


(add-to-list 'company-backends '(company-dabbrev company-yasnippet))
