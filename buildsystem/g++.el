
(defclass buildsystem/g++ ()
  ((name :initform "g++")
   (main-source :initform nil)
   (options :initform ("-Wall" "-std=c++14")) ))

(defmethod detect ((obj buildsystem/g++))
  (let ((detected (eq major-mode 'c++-mode)))
    (if (identity detected)
        (set-slot-value obj 'main-source buffer-file-name)
        )
    (identity detected) ))

(defmethod build_command ((obj buildsystem/g++) _)
  (concat "g++"
          " " (mapconcat 'identity (slot-value obj 'options) " ")
          " " (slot-value obj 'main-source) ))
