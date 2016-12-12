;;; --------------------------------
;;; Allegro CL 

#+excl (setf (excl:package-definition-lock (find-package ':common-lisp)) nil)

#+excl (when (eq excl:*current-case-mode* :case-sensitive-upper)
	 (warn "you've chosen the one case (sensitive-upper) that is incompatble with clm -- changing to insensitive-upper")
	 (excl:set-case-mode :case-insensitive-upper))

#+(and excl windoze) (chdir clm-directory)
#+(and excl windoze) (setf *default-pathname-defaults* (excl:current-directory))

;;; in ACL 8.2/Linux, there can be a problem with selinux leading to the loader complaint:
;;;   "libclm.so: cannot restore segment prot after reloc: Permission denied."
;;; it's a bother to call chcon -t textrel_shlib_t libclm.so by hand all the time, so
;;;   in that case define use-chcon t below

(defvar use-chcon nil)

#+excl (progn

  (require :foreign)
  (setf excl:*global-gc-behavior* :auto) ;turn off the gc tenure message
  (setf excl:*redefinition-warnings* nil)

  (defun compile-and-load (name)
    (let* ((dir clm-directory)
	   (bindir clm-bin-directory)
	   (cname (concatenate 'string dir name ".lisp"))
	   (lname #-allegro-cl-lite (concatenate 'string bindir name "." excl:*fasl-default-type*)
		  #+allegro-cl-lite cname))
      (when (not (probe-file cname)) (setf cname (concatenate 'string dir name ".ins")))
      #-allegro-cl-lite
      (if (probe-file cname)
	  (if (or (not (probe-file lname))
		  (> (file-write-date (truename cname)) (file-write-date (truename lname))))
	      (handler-bind ((excl:compiler-no-in-package-warning #'(lambda (c) (declare (ignore c)) (muffle-warning)))
			     (excl:compiler-undefined-functions-called-warning #'(lambda (c) (declare (ignore c)) (muffle-warning)))
			     (style-warning #'(lambda (c) (declare (ignore c)) (muffle-warning))))
			    (compile-file cname :output-file lname))))
      (handler-bind ((simple-warning #'(lambda (c) (declare (ignore c)) (muffle-warning)))) (load lname))
      ))

  #-loop (let ((*default-pathname-defaults* (parse-namestring ""))) (require :loop))

  (setq sys:*source-file-types* '("cl" "lisp" nil "ins" "cm" "clm" "cmn"))

  (setq sys:*load-search-list*		;don't want to compile note-lists
    (append sys:*load-search-list* (list (make-pathname :type "ins")
					 (make-pathname :type "cm")
					 (make-pathname :type "clm") 
					 (make-pathname :type "cmn") 
					 )))
  )


;;; ------------------------------
;;; CCL


#+openmcl 
(unless (get-dispatch-macro-character #\# #\,) 
  ;; since the "#," dispatch macro used by walk.lisp is not part of 
  ;; ANSI CL it was (rather gratuitously) removed from openmcl 1.0. 
  ;; we add it back here. 
  (set-dispatch-macro-character 
   #\# 
   #\, 
   #'(lambda (stream subchar numarg) 
       (let* ((sharp-comma-token ccl::*reading-for-cfasl*)) 
         (if (or *read-suppress* (not ccl::*compiling-file*) (not sharp-comma-token)) 
             (ccl::read-eval stream subchar numarg) 
             (progn 
               (ccl::require-no-numarg subchar numarg) 
               (list sharp-comma-token (read stream t nil t))))))))

#+openmcl

;(in-package :clm)

(defun compile-and-load (name)
  (let* ((dir clm-directory)
	 (bindir clm-bin-directory)
	 (cname (concatenate 'string dir name ".lisp"))
	 (lname (concatenate 'string bindir name "." (pathname-type (compile-file-pathname cname)))))
    (when (not (probe-file cname)) (setf cname (concatenate 'string dir name ".ins")))
    (if (probe-file cname)
	(if (or (not (probe-file lname))
		(> (file-write-date (truename cname)) (file-write-date (truename lname))))
	    (compile-file cname :output-file lname)))
    (load lname)))


;;; --------------------------------
;;; CMU CL

#+cmu (declaim (optimize (extensions:inhibit-warnings 3))) 
#+cmu (setf extensions::*gc-verbose* nil)
#+cmu (setf *compile-print* nil)
#+cmu (setf *compile-verbose* nil)

#+cmu (defun compile-and-load (name)
	(let* ((dir clm-directory)
	       (bindir clm-bin-directory)
	       (cname (concatenate 'string dir name ".lisp"))
	       (lname (concatenate 'string bindir name "." (c:backend-fasl-file-type c:*backend*))))
	  (when (not (probe-file cname)) (setf cname (concatenate 'string dir name ".ins")))
	  (if (probe-file cname)
	      (if (or (not (probe-file lname))
		      (> (file-write-date (truename cname)) (file-write-date (truename lname))))
		  (compile-file cname :output-file lname)))
	  (load lname)))



;;; --------------------------------
;;; Steel Bank CL

#+sbcl (setf *compile-print* nil)
#+sbcl (setf *compile-verbose* nil)
#+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

#+sbcl (defun compile-and-load (name)
	(let* ((dir clm-directory)
	       (bindir clm-bin-directory)
	       (cname (concatenate 'string dir name ".lisp"))
	       (lname (concatenate 'string bindir name ".fasl")))
	  (when (not (probe-file cname)) (setf cname (concatenate 'string dir name ".ins")))
	  (if (probe-file cname)
	      (if (or (not (probe-file lname))
		      (> (file-write-date (truename cname)) (file-write-date (truename lname))))
		  (handler-bind ((warning #'(lambda (c) (declare (ignore c)) (muffle-warning)))
				 (style-warning #'(lambda (c) (declare (ignore c)) (muffle-warning))))
		    (format t "~%;compiling ~A" cname)
		    (force-output)
		    (compile-file cname :output-file lname))))
	  (handler-bind ((warning #'(lambda (c) (declare (ignore c)) (muffle-warning))))
	    (format t "~%;loading ~A" lname)
	    (load lname)
	    (force-output)
	    )))

;;; --------------------------------
;;; Lispworks

#+lispworks(setf *compile-print* nil)
#+lispworks(setf *compile-verbose* nil)
#+lispworks(declaim (muffle-warning compiler-note))

#+lispworks(defun compile-and-load (name)
	     (let* ((dir clm-directory)
		    (bindir clm-bin-directory)
		    (cname (concatenate 'string dir name ".lisp"))
		    (lname (concatenate 'string bindir name "." (pathname-type (cl-user::compile-file-pathname "")))))
	       (when (not (probe-file cname)) (setf cname (concatenate 'string dir name ".ins")))
	       (if (probe-file cname)
		   (if (or (not (probe-file lname))
			   (> (file-write-date (truename cname)) (file-write-date (truename lname))))
		       (handler-bind ((warning #'(lambda (c) (declare (ignore c)) (muffle-warning)))
				      (style-warning #'(lambda (c) (declare (ignore c)) (muffle-warning))))
			 (format t "~%;compiling ~A" cname)
			 (force-output)
			 (compile-file cname :output-file lname))))
	       (handler-bind ((warning #'(lambda (c) (declare (ignore c)) (muffle-warning))))
		 (format t "~%;loading ~A" lname)
		 (load lname)
		 (force-output)
		 )))


;;; --------------------------------
;;; Clisp

#+clisp (defun compile-and-load (name)
	  (handler-bind
	      ((t #'(lambda (c)
		      (when (find-restart 'continue)
			(invoke-restart 'continue)))))
	    (let* ((dir clm-directory)
		 (bindir clm-bin-directory)
		 (cname (concatenate 'string dir name ".lisp"))
		 (lname (concatenate 'string bindir name ".fas")))
	    (when (not (probe-file cname)) (setf cname (concatenate 'string dir name ".ins")))
	    (if (probe-file cname)
		(if (or (not (probe-file lname))
			(> (file-write-date (truename cname)) (file-write-date (truename lname))))
		    (compile-file cname :output-file lname)))
	    (load lname))))
	  
