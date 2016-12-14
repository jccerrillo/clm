;;;
;;; This modifies the definitions of play and stop-playing from sound.lisp to be able to use something like apple's afplay
;;; by evaluating (setf *clm-player* "afplay")
;;;
;;; sndplay changes the samplerate of some soundcards when called, thus plays sounds with wrong frequencies
;;; 

(in-package :clm)

#+(or (and ccl mac-osx) (and lispworks mac-osx) (and sbcl mac-osx))
(defparameter *dac-pid* nil)
(defvar last-dac-filename #-(or cmu sbcl openmcl) nil #+(or cmu sbcl openmcl) *clm-file-name*)

#+(or (and ccl mac-osx) (and lispworks mac-osx) (and sbcl mac-osx))
(defun play (&optional name-1 &key start end (wait *clm-dac-wait-default*))
  #-(or excl openmcl cmu sbcl) (declare (ignore wait))
  (clm-initialize-links)
  (let ((filename (if name-1
		      (filename->string #-excl (translate-logical-pathname (->pathname (filename->string name-1)))
				  #+excl (full-merge-pathnames name-1 *clm-file-name*) ; excl screws up in translate-logical-pathname
				  )
		    last-dac-filename)))
    (if (not filename)
	(warn "no previous file to play")
      (if (functionp *clm-player*) ;for example (setf *clm-player* #'sl-dac) -- sl-dac is in clm3.lisp (sl-dac-1 in ffi.lisp)
	  (funcall *clm-player* filename)
	(let* ((sndplay (or *clm-player*
			    (concatenate 'string *clm-binary-directory* "sndplay" #+windoze ".exe")))
	       (command-args
		;; this is a nightmare because every lisp handles external programs differently,
		;;   and we're trying to allow the user to stop the dac prematurely
		;;
		;; EXCL -- use vector
		#+(and excl (not windoze))
		(let ((args (list sndplay sndplay filename)))
		  (if (and (not *clm-player*)
			   (or start end))
		      (progn
			(if start (setf args (append args (list "-start" (format nil "~A" start)))))
			(if end (setf args (append args (list "-end" (format nil "~A" end)))))))
		  (apply #'vector args))
		;;
		;; CMU, SBCL, and OPENMCL -- use list
		#+(or cmu sbcl openmcl)
		(let ((args (list filename)))
		  (if (not *clm-player*)
		      (progn
			(if start 
			    (setf args (append args (list "-start" (format nil "~A" start)))))
			(if end 
			    (setf args (append args (list "-end" (format nil "~A" end)))))
			#+mac-osx (if (= *clm-output-properties-mutable* 0)
				      (setf args (append args (list "-mutable 0"))))
			))
		  args)
		;;
		;; ELSE (not always relevant)
		#-(or cmu sbcl openmcl (and excl (not windoze)))
		(format nil "~A~A~A"
			filename
			(if start (format nil " -start ~A" start) "")
			(if end (format nil " -end ~A" end) ""))
		))
	  
	  ;; following lines ommited to be able to set "afplay"

	  ;;(when (not (probe-file sndplay))
	  ;;  (setf sndplay "sndplay")) ; hope there's a system version, I guess

	  ;;
	  ;; SNDPLAY
	  #-(or excl openmcl cmu sbcl lispworks)
	  (run-in-shell sndplay command-args)
	  #+ccl
	  ;; modified to be able to kill process
	  (progn
	    (when *dac-pid* (ccl:run-program "kill" (list (write-to-string *dac-pid*))))
	    (setf *dac-pid* (ccl:external-process-id
			     (ccl:run-program sndplay command-args
					      :output t
					      :wait wait
					      :status-hook #'(lambda (x) (progn
									  (print x)
									  (setf *dac-pid* nil)))))))
	  #+(and excl windoze)
	  (run-in-shell sndplay command-args)
	  #+(and excl (not windoze))
	  (progn
	    ;; this dubious business tested slightly in Linux and SGI
	    (when *dac-pid* (loop while (numberp (system:os-wait))))
	    (setf *dac-pid* (nth-value 2 (excl:run-shell-command command-args :wait wait))))
	  #+cmu
	  ;; *dac-pid* is not really a pid, it is a cmucl process
	  (progn
	    ;; wait for a previous play command
	    (when *dac-pid* (ext:process-wait *dac-pid*))
	    (setf *dac-pid* (ext:run-program sndplay command-args :wait wait)))
	  #+sbcl
	  (progn
	    ;; wait for a previous play command
	    (if (equal *clm-player* "afplay")
		(setf sndplay "/usr/bin/afplay"))
	    (when *dac-pid* (sb-ext:process-wait *dac-pid*))
	    (setf *dac-pid* (sb-ext:run-program sndplay
						(list 
						 (concatenate
						  'string
						  (namestring (user-homedir-pathname))
						  filename))
						:wait wait :output t)))
	  #+lispworks
	  (progn
	    (when *dac-pid* (sys:run-shell-command
			     (concatenate 'string "kill " (write-to-string *dac-pid*))
			     :wait nil))
	    (setf *dac-pid* (multiple-value-bind (out err pid)
				(sys:run-shell-command (concatenate 'string sndplay " " filename)
						       :wait nil
						       :output :stream
						       :if-input-does-not-exist nil)
			      (values pid))))
	  (if filename (setf last-dac-filename filename))
	  (format t "~a ~a" sndplay filename))))
    last-dac-filename))

#+(or (and ccl mac-osx) (and lispworks mac-osx) (and sbcl mac-osx))
(defun stop-playing ()
    #+excl (progn
	   (print *dac-pid*)
	   (force-output)
	   (when *dac-pid*
	     #-windoze (excl:shell (format nil "kill ~D" *dac-pid*))
	     (system:os-wait)
	     (setf *dac-pid* nil)))
  #+cmu
  (when *dac-pid*
    (print (ext:process-pid *dac-pid*))
    (force-output)
    (ext:process-kill *dac-pid* :sigterm)
    (ext:process-wait *dac-pid*)
    (setf *dac-pid* nil))
  #+sbcl
  (when *dac-pid*
    (print (sb-ext:process-pid *dac-pid*))
    (force-output)
    (sb-ext:process-kill *dac-pid* sb-unix::sigterm) ; in sb-unix as of 0.9.8 => use 15 if they move it again
    (sb-ext:process-wait *dac-pid*)
    (setf *dac-pid* nil))
  #+lispworks
  (when *dac-pid*
    (sys:run-shell-command (concatenate 'string "kill " (write-to-string *dac-pid*))
			   :wait nil)
    (setf *dac-pid* nil))
  #+ccl
  (when *dac-pid*
    (ccl:run-program "kill" (list (write-to-string *dac-pid*)))
    (setf *dac-pid* nil))
  )


