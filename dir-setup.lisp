;;;
;;;  setup clm-directory 
;;;

(proclaim '(special clm-directory clm-bin-directory))

#-quicklisp
(if (not (boundp 'clm-directory)) 
    (setf clm-directory 
      (namestring
       (truename 
	(directory-namestring (or *load-pathname* "./"))))))


#+openmcl (if (and (stringp clm-directory)
		   (> (length clm-directory) 0)
		   (not (char= (elt clm-directory (1- (length clm-directory))) #\/)))
	      (setf clm-directory (concatenate 'string clm-directory "/")))

#+quicklisp
(setf clm-directory (namestring (ql:where-is-system :clm)))
(if (not (boundp 'clm-bin-directory)) (setf clm-bin-directory clm-directory))
