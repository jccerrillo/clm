
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
