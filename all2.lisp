(in-package :clm)

;;; --------------------------------
;;; code walker, package definitions, etc

#-(or excl sbcl lispworks)
(progn
  (when (find-symbol "WALK-FORM-EXPAND-MACROS-P" :walker)
    (set (find-symbol "WALK-FORM-EXPAND-MACROS-P" :walker) t))
  #+(and (or clisp cmu) ansi-cl) (load (concatenate 'string clm-directory "special-form-for-cmu.cl"))
  )

#+cmu (setf walker:walk-form-expand-macros-p t)
;;; in sbcl it's in the SB-WALKER package (&#%@^$)
#|
(compile-and-load "clm-package")
(compile-and-load "initmus")
|#

(setf clm::*clm-binary-directory* (namestring (truename clm-bin-directory)))
(setf clm::*clm-source-directory* (namestring (truename clm-directory)))

(setf clm::*clm-compiler-name* c-compiler)
#+(or excl cmu sbcl openmcl clisp lispworks) (setf clm::*so-ext* *shared-object-extension*)

#+excl (setf clm::*use-chcon* use-chcon)
#+excl (if use-chcon (excl:shell (format nil "chcon -t textrel_shlib_t ~A" (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*))))
#+excl (load (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*))
#+lispworks (fli:register-module (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*))
#+cmu (ext:load-foreign (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*))
#+sbcl (sb-alien:load-shared-object (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*))
#+(and clozure darwin)
;;; workaround for a nasty Mac OS X 10.6 "misfeature"
;;; the CoreFoundation libary is loaded indirectly when libclm.dylib is loaded
;;; 10.6 disallows loading CoreFoundation outside the main thread
;;; Clozure CL uses the main thread for housekeeping and spawns an additional listener thread
;;; so we must force CoreFoundation to load from the main thread
;;; see http://www.clozure.com/pipermail/openmcl-devel/2010-February/011118.html
(let ((core-foundation "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation"))
  (when (not (ccl::shared-library-with-name core-foundation))
	(let* ((s (make-semaphore)))
	  (process-interrupt
	   ccl::*initial-process*
	   (lambda ()
		 (open-shared-library core-foundation)
		 (ccl:signal-semaphore s)))
	  (ccl:wait-on-semaphore s))))

#+openmcl (ccl:open-shared-library (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*))
#|
#+openmcl (compile-and-load "mcl-doubles")

(compile-and-load "sndlib2clm")
(compile-and-load "defaults")
|#

;;; -------- sndplay
;;; make a program named sndplay (and sndinfo and audinfo?) that can play most sound files 
;;;

; TODO
; a new configure and compiling of libraries is needed if one is working on 32bit lisp on 64 bit machine (lw-personal)
; otherwise sndplay is not properly built
;


#+lispworks-32bit
(defun touch-file (file)
  (sys:run-shell-command (concatenate 'string "touch " file)))

#+lispworks-32bit
(progn
    (let* ((c-file-names '("io" "headers" "audio" "sound" "clm" "cmus"))
	   (c-files (mapcar #'(lambda (x) (concatenate 'string clm-directory x ".c")) c-file-names)))
      (mapcar #'touch-file c-files)
      (setf configure (concatenate 'string "cd " clm-directory " && ./configure --quiet"))
      ;#+lispworks-32bit (setf configure (concatenate 'string configure " --host=i686"))
      (format t ";   running ~A~%" configure)
      #+lispworks (sys::call-system-showing-output configure)
      (defvar *cflags* (concatenate 'string " -I" clm-directory " -O2"
				  #-windoze " -g"
				  #+(or macosx (and mac-osx openmcl)) " -no-cpp-precomp"
				  ))
      (defvar *csoflags* (concatenate 'string
				      #+macosx " -shared"
				      ))
    (format t "~%Compiling headers for lw agian~%")
    (cc-it "io")
    (cc-it "headers")
    (cc-it "audio")
    (cc-it "sound")
    (cc-it "clm")
    (cc-it "cmus"))
#|
    (let ((shared-name (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*)))
      (princ (format nil "; Creating ~S~%" shared-name))
      (setf someone-loaded t)
      (let ((str (concatenate 
		  'string
		  *ld* " "
		  #-(and cmu freebsd) *csoflags* " "
		  #-windoze "-o " #+windoze "-Fe"
		  shared-name " "
		  clm-bin-directory "headers" *obj-ext* " "
		  clm-bin-directory "audio" *obj-ext* " "
		  clm-bin-directory "io" *obj-ext* " "
		  clm-bin-directory "sound" *obj-ext* " "
		  clm-bin-directory "clm" *obj-ext* " "
		  clm-bin-directory "cmus" *obj-ext* " "
		  #-(or windoze (and openmcl linuxppc-target) (and linux (or cmu sbcl acl-50))) " -lm -lc"
		  #+(or mac-osx os-macosx) " -framework CoreAudio"
		  )))
	(format t ";;~A~%" str)
	(force-output)
	#+lispworks (sys::run-shell-command str)
	(if (not (probe-file shared-name))
	    (format t "~S was not created?  Perhaps there was a C compiler or loader error.~%~
                     You might try the following command in a terminal to see what happened: ~S~%"
		    shared-name
		  str))))
|#
    )



#+windoze
  (when (or (not (probe-file "sndplay.exe"))
	    (> (file-write-date "sndplay.c") (file-write-date "sndplay.exe")))
    #+excl (excl:shell (format nil "cl sndplay.c -I. -DMUS_WINDOZE libclm.lib~%"))
    )

#-windoze
  (let ((prog-name (concatenate 'string clm::*clm-binary-directory* "sndplay"))
	(source-name (concatenate 'string clm::*clm-source-directory* "sndplay.c")))
    (when (or (not (probe-file prog-name))
	      (> (file-write-date source-name) (file-write-date prog-name)))
      (let ((dacstr (concatenate 'string
				 c-compiler
				 *cflags*
				 " "
				 clm::*clm-binary-directory* "headers.o "
				 clm::*clm-binary-directory* "audio.o "
				 clm::*clm-binary-directory* "io.o "
				 clm::*clm-binary-directory* "sound.o "
				 clm::*clm-binary-directory* "clm.o "
				 source-name
				 " -o "
				 prog-name
				 #+sgi " -laudio"
				 #+alsa " -lasound"
				 #+jack " -ljack -lsamplerate -lasound"
				 ;#+sb-thread " -lpthread"
				 #+(or macosx mac-osx) " -framework CoreAudio"
				 " -lm"
				 )))
	(princ (format nil "; Building sndplay program: ~S~%" prog-name)) (force-output)
	#+excl (excl:shell dacstr)
	#+cmu (extensions:run-program "/bin/csh" (list "-fc" dacstr) :output t)
	#+lispworks (sys::run-shell-command dacstr)
	#+openmcl (ccl:run-program "/bin/csh" (list "-fc" dacstr) :output t)
	#+sbcl (sb-ext:run-program "/bin/csh" (list "-fc" dacstr) :output t)
	#+clisp (ext::shell dacstr)
	)))
