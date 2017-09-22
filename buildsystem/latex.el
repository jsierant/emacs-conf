
(defclass buildsystem/latex ()
  ((name :initform "latex")
   (main-source :initform nil)
   (build-dir :initform "./build")
   (options :initform ("-file-line-error" "-interaction=nonstopmode")) ))

(defmethod detect ((obj buildsystem/latex))
  (let ((detected (eq major-mode 'latex-mode)))
    (if (identity detected)
        (progn
          (set-slot-value obj 'main-source buffer-file-name)
          (unless (file-exists-p (slot-value obj 'build-dir))
            (make-directory (slot-value obj 'build-dir)))
          )
      )
    (identity detected) ))

(defmethod build_command ((obj buildsystem/latex) _)
  (concat "pdflatex"
          " -output-directory=" (slot-value obj 'build-dir)
          " " (mapconcat 'identity (slot-value obj 'options) " ")
          " " (slot-value obj 'main-source) ))
