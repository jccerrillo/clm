(defparameter *clm-src-dir* nil)
(setf *clm-src-dir*
      (concatenate 'string (namestring (car ql:*local-project-directories*)) "clm/"))


(setf clm-directory *clm-src-dir*)
(setf clm-bin-directory *clm-src-dir*)
