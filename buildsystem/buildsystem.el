
;; frontend
(setq compilation-read-command nil)
(defun compile-in-bottom-window ()
  "Compilation in bottom window"
  (interactive)
  (setq-local compile-buffer-name  "*compilation*")
  (if (eq (get-buffer-window compile-buffer-name) nil)
      (progn
        (setq compile-window
              (split-window (frame-root-window)
                            (floor (* (window-total-height (frame-root-window)) 0.8)) 'below))
        (setq compile-buffer (get-buffer-create compile-buffer-name))
        (set-window-buffer compile-window compile-buffer)
        (set-window-point compile-window (point-max))
        )
    )
  (call-interactively 'compile) )

(global-set-key [f8] 'buildsystem/make-targets)


;; backends
(defmethod build_command (_ targets) (concat "make " targets))
(defmethod setup_command (_) "")
(defmethod cwd (_) default-directory)

(load-file "~/.emacs.d/buildsystem/make.el")
(load-file "~/.emacs.d/buildsystem/cmake.el")
(load-file "~/.emacs.d/buildsystem/g++.el")

(setq buildsystem/backends
      (list
       (make-instance 'buildsystem/make)
       (make-instance 'buildsystem/cmake)
       (make-instance 'buildsystem/g++) ))


;; system
(defvar buildsystem/active nil)


(defun buildsystem/detect-impl (types)
  (when types
    (let ((type (car types)))
      (if (detect type)
          (progn
            (setq buildsystem/active type)
            (message (concat "BuildSystem: activating "
                             (slot-value type 'name) " buildsystem")) )
        (buildsystem/detect-impl (cdr types)) ))))

(defun buildsystem/detect ()
  (buildsystem/detect-impl buildsystem/backends) )

(defun buildsystem/activate-impl (types name)
  (when types
    (let ((type (car types)))
      (if (string= name (slot-value type 'name))
          (progn
            (setq buildsystem/active type)
            (message (concat "BuildSystem: activating " name " buildsystem")) )
        (buildsystem/activate-impl (cdr types) name) ))))


(evil-define-command buildsystem/activate (name)
  (interactive "<sh>")
  (buildsystem/activate-impl buildsystem/backends name) )
(evil-ex-define-cmd "bsa[ctivate]" 'buildsystem/activate)


(defun buildsystem/run (targets)
  (if (eq buildsystem/active nil)
      (message "BuildSystem: no buildsystem type set/detected!")
    (let ((compile-command (build_command buildsystem/active targets))
          (default-directory (cwd buildsystem/active)))
      (message (concat "BuildSystem: building targets [ " targets " ]"))
      (compile-in-bottom-window) )))


(defvar buildsystem/targets "")
(evil-define-command buildsystem/set-targets(targets)
  (interactive "<sh>")
  (setq buildsystem/targets targets) )

(defun buildsystem/make-targets ()
  (interactive)
  (buildsystem/run buildsystem/targets) )

(evil-ex-define-cmd "bst[argets]" 'buildsystem/set-targets)


(evil-define-command buildsystem/setup ()
  (interactive)
  (if (eq buildsystem/active nil)
      (message "BuildSystem: no buildsystem type set/detected!")
    (let ((compile-command (setup_command buildsystem/active))
          (default-directory (cwd buildsystem/active)))
      (if (eq compile-command "")
          (message (concat "BuildSystem: no setup for "
                           (slot-value buildsystem/active 'name) " buildsystem"))
        (progn
          (message (concat "BuildSystem: setting up "
                           (slot-value buildsystem/active 'name)
                           " buildsystem at " default-directory))
          (compile-in-bottom-window) )))))

(evil-ex-define-cmd "bss[etup]" 'buildsystem/setup)


(evil-define-command buildsystem/compile (targets)
  (interactive "<sh>")
  (buildsystem/run targets) )

(evil-ex-define-cmd "bsc[ompile]" 'buildsystem/compile)


(run-at-time "1 sec" nil 'buildsystem/detect)
