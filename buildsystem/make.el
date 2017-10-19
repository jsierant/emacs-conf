
(defclass buildsystem/make ()
  ((name :initform "make")))

(defmethod detect ((obj buildsystem/make))
 (file-readable-p "./Makefile"))
