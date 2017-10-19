
(defclass buildsystem/cmake ()
  ((name :initform "cmake")
   (builddir :initform "build")
   (options :initform ("-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")) ))

(defmethod detect ((obj buildsystem/cmake))
 (file-readable-p "./CMakeLists.txt"))

(defmethod setup_command ((obj buildsystem/cmake))
  (concat "cmake "
          (mapconcat
           'identity
           (slot-value obj 'options) " ")
          " "
          (projectile-project-root) (slot-value obj 'builddir)))

(defmethod cwd ((obj buildsystem/cmake))
  (concat (projectile-project-root) (slot-value obj 'builddir)))
