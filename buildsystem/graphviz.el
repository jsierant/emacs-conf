
(defclass buildsystem/graphviz ()
  ((name :initform "graphviz")
   (main-source :initform nil) ))

(defmethod detect ((obj buildsystem/graphviz))
  (let ((detected (eq major-mode 'graphviz-dot-mode)))
    (if (identity detected)
        (set-slot-value obj 'main-source buffer-file-name)
        )
    (identity detected) ))

(defmethod build_command ((obj buildsystem/graphviz) _)
  (concat "dot -Tpdf"
          " " (slot-value obj 'main-source)
          " > " (file-name-sans-extension (slot-value obj 'main-source)) ".pdf" ))
