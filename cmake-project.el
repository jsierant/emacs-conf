
;;; Code:
(require 'projectile)

(defvar cmake-project-binary-dir "build"
  "CMake projects binary directory."
  )

(defun cmake-project-add-cdb ()
  "Add compilation database to rdm."
  (interactive)
  (make-process
   :name "rcJ"
   :command (list "/usr/bin/rc" "-J"
                  (concat projectile-project-root
                          "/" cmake-project-binary-dir
                          "/compile_commands.json"))
   )
  )

(provide 'cmake-project)
;;; cmake-project.el ends here
