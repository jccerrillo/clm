;;;
;;;  setup clm-directory 
;;;

(defparameter *clm-src-dir*
  (asdf:component-pathname (asdf:find-system :clm))
  "Directory holding CLM source files")

(defparameter clm-directory (namestring *clm-src-dir*))
(defparameter clm-bin-directory (namestring *clm-src-dir*))

#+lispworks
(change-directory clm-directory)
