;;; Load file for CLM.
;;; 
;;; The CLM sources are found on the current directory unless the variable clm-directory is set.
;;; The binaries (and compiled C modules, where applicable) are written to clm-bin-directory, if bound.
;;; See make-clm.cl for one example of how to set everything up.
;;;
;;; for example, say we've untarred clm-4.tar to the current directory,
;;; but we want the .fasl and .o files written to /zap -- start lisp,
;;; (setf clm-directory "") (setf clm-bin-directory "/zap/") (load "all.lisp")
;;;
;;; (setf *clm-player* (concatenate 'string clm-bin-directory "sndplay"))
;;;   causes CLM to use its local version of sndplay
;;;
;;; to force the configure script to run even if mus-config.h exists, (pushnew :reconfigure *features*)


;;;
;;;
;;;  Esta cerca, falta ver por que no compila las librerias c
;;;
;;;



;(in-package :clm)

(pushnew :clm2 *features*) ; for CM
(pushnew :clm3 *features*)
(pushnew :clm4 *features*)

#+(and openmcl (not linux) (not linux-target) (not linuxppc-target)) (pushnew :mac-osx *features*)
#+(or darwin macosx) (pushnew :mac-osx *features*) ; acl uses macosx

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

#|
  The various setup possibilities are handled via Lisp's *features* list --
  we push something on that list to tell Lisp what to include in the current setup.
  The CLM *features* are:

     :reconfigure    force configure script to run again

The *features* that CLM adds itself are:

     :clm
     :linux           if linux86
     :windoze         windows
     :little-endian (:big-endian) if not already present
     :acl-50          Franz changed the foreign function interface in ACL 5
     :acl-60          Franz changed the foreign function interface in ACL 6
     :acl-61 :acl-62  and so on...
     
  The other *features* that CLM currently notices (leaving aside walk.lisp and loop.lisp) are:

     :sgi             SGI
     :sun,solaris     Sun (sometimes ignored or removed by clm)
     :hpux            HPUX.
     :i386, i486, pc386  386-style processor (includes 486 and Pentium)
     :freebsd         ACL, CmuCL on FreeBSD.
     :linux86         Linux (in excl, if this is present, and :little-endian, we remove :sun from *features*)
     :cmu             CMU-CL
     :excl            Franz Inc's Lisp (Allegro CL)
     :alsa            use ALSA, rather than OSS (Linux)
     :jack            use the Jack Audio Connection Kit driver (linux), also requires libsamplerate
     :powerpc or :ppc Macintosh PowerPC
     :irix            SGI.
     :cltl2           Current lisp is a version 2 CL -- that is, it uses the later package syntax, etc
     :x3j13           A late enough version of the upcoming ANSI CL to have read-sequence
     :little-endian   Underlying machine is little endian (also :big-endian)
     :dlfcn           Foreign code loader is dlopen-related (SGI, Linux)
     :ccrma           Special ACL 4.3 output type for us (avoid collision with ACL 4.3 on SGI and Linux)
                      and use /zap rather than /tmp for clm/snd communication paths
     :win98           Windows 98 where longer file names are apparently accessible
     :mswindows       ACL under Windows
     :allegro-cl-lite ACL with no compiler (the free version of ACL under Windoze)
     :mac-osx         Mac OSX -- this might need to be pushed explicity before loading this file
     :sbcl            Steel Bank CL (cmu-cl descendent)
     :openmcl         OpenMCL
     :darwin          Mac OSX
     :macosx          ACL 7/8 OSX
     :clisp           Clisp
|#

#+(and (or solaris sparc sunos) (not sun)) (pushnew :sun *features*)
#+(or mswindows win98 win32) (pushnew :windoze *features*)
#+(and linux86 (not linux)) (pushnew :linux *features*)
#+(and excl linux86 little-endian) (setf *features* (remove :sun *features*))
#+bsd386 (pushnew :linux *features*)
;;; acl-50 et all pushed in acl.cl
#+irix (pushnew :sgi *features*)
#+(or x3j13 draft-ansi-cl-2) (pushnew :cltl2 *features*)
#+(and cmu freebsd) (pushnew :linux *features*)
#+(and clisp unix macos) (pushnew :mac-osx *features*)


#-windoze (if (and (stringp clm-bin-directory)
		   (> (length clm-bin-directory) 1)
		   (not (char= (elt clm-bin-directory (1- (length clm-bin-directory))) #\/)))
	      (setf clm-bin-directory (concatenate 'string clm-bin-directory "/")))

#+excl (load (concatenate 'string clm-directory "acl.cl"))

(let ((oops (remove-duplicates (intersection *features* (list :excl :cmu)))))
  (if (and oops (> (length oops) 1))
      (warn "somehow all.lisp thinks ~D different lisps are running here: ~A" (length oops) oops)))

(let ((oops (remove-duplicates (intersection *features* (list :sun :linux :sgi :hpux)))))
  (if (and oops (> (length oops) 1))
      (warn "somehow all.lisp thinks ~D different machines are running here: ~A" (length oops) oops)))

#+(and (not (or big-endian little-endian)) (or (and openmcl x86-target) x86-64 pc386 bsd386 i386 i486 i586 i686 x86 win32)) (pushnew :little-endian *features*)
#+(and (not (or big-endian little-endian)) (or (and openmcl (not x86-target)) sgi hpux powerpc ppc)) (pushnew :big-endian *features*)
#+(and (not (or big-endian little-endian)) excl)
  (pushnew
   (let ((x '#(1)))
     (if (not (= 0 (sys::memref x #.(sys::mdparam 'comp::md-svector-data0-adj) 0 :unsigned-byte)))
	 :little-endian
       :big-endian))
   *features*)


(pushnew :clm *features*)

;#+windoze (excl:shell "cp config.windoze mus-config.h")

(defvar reconfigure #+reconfigure t #-reconfigure nil)

(if (not (probe-file (concatenate 'string clm-directory "mus-config.h")))
    (setf reconfigure t))
(defvar configure nil)

#+(and linux (not oss) (not jack) (not alsa)) (pushnew :alsa *features*)
#+(and clisp unix (not macos) (not sun) (not oss) (not jack) (not alsa)) (pushnew :alsa *features*)

(if reconfigure
    (progn
      (setf configure (concatenate 'string "cd " clm-directory " && ./configure --quiet"))
      
      #+(and clozure darwinx8632-target)
      (setf configure (concatenate 'string configure " CLFAGS='-arch i386' LDFLAGS='-arch i386'"))
      #+lispworks-32bit (setf configure (concatenate 'string configure " --host=i686"))
      #+alsa (setf configure (concatenate 'string configure " --with-alsa"))
      #+oss (setf configure (concatenate 'string configure " --with-oss"))
      #+jack (setf configure (concatenate 'string configure " --with-jack"))
      ;#+sb-thread (setf configure (concatenate 'string configure " --enable-threads"))
      ;removed 28-Sep-11
      
      (format t ";   running ~A~%" configure)
      #+excl (excl:run-shell-command configure :wait t)
      #+sbcl (sb-ext:run-program "/bin/csh" (list "-fc" configure) :output t)
      #+lispworks (sys::call-system-showing-output configure)
      #+cmu (extensions:run-program "/bin/csh" (list "-fc" configure) :output t)
      #+openmcl (ccl:run-program "/bin/csh" (list "-fc" configure) :output t)
      #+clisp (ext::shell configure)
      )
  #-openmcl (format t ";   using existing configuration file mus-config.h~%")
  )

(defvar *shared-object-extension* 
  #+dlfcn "so" 
  #+dlhp "sl" 
  #+(or windoze dlwin) "dll" 
  #+(or (and excl macosx) (and openmcl (not linux-target) (not linuxppc-target))) "dylib"
  #-(or dlfcn dlhp dlwin windoze (and excl macosx) (and openmcl (not linux-target) (not linuxppc-target))) "so"
  )

(defvar pre-c-name #-windoze "saved-" #+windoze "n")
(defvar *obj-ext* #-windoze ".o" #+windoze ".obj")

(defvar *cflags* (concatenate 'string " -I" clm-directory " -O2"
			      #-windoze " -g"
			      #+(and excl linux debug) " -Wall"
			      #+lispworks-32bit " -m32"
			      #+(and excl macosx) " -dynamic"
			      #+(and clozure darwinx8632-target) " -arch i386"
			      #+(or macosx (and mac-osx openmcl)) " -no-cpp-precomp"
			      #+clisp " -fPIC"
			      #+(or netbsd x86-64) " -fPIC"
			      #+(and sgi acl-50) " -n32 -w"
			      #+(and excl freebsd) " -fPIC -DPIC"
			      ))

#+alsa (if (probe-file "/usr/include/alsa/asoundlib.h") (setf *cflags* (concatenate 'string *cflags* " -DHAVE_ALSA_ASOUNDLIB_H")))

(defvar *csoflags* (concatenate 'string
				#+sgi " -shared -all"
				#+(or (and linux (not lispworks-32bit)) clisp ) " -shared -fPIC"
				#+(or linuxppc-target (and mac-osx (not excl) (not cmu) (not sbcl) (not openmcl))) " -shared -whole-archive"
				#+(and lispworks-32bit) "-L/usr/lib -m32 --shared"
				#+(and openmcl linux-target) " -shared"
				#+(and mac-osx cmu) " -dynamiclib"
				#+(and excl macosx) " -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress"
				#+hpux " +z -Ae +DA1.1" ;" -b"?
				#+sun " -G"
				#+windoze " -D_MT -MD -nologo -LD -Zi -W3"
				#+(and excl freebsd) " -Bshareable -Bdynamic"
				#+netbsd " -shared"
				))

(defvar *lib-ext* #+windoze "lib" #-windoze "a")
(defvar *ld* #+windoze "cl" 
             #+(and (or lispworks-32bit clisp sbcl openmcl (and mac-osx cmu)) (not linuxppc-target)) "gcc" 
	     #-(or (and (or lispworks-32bit clisp sbcl openmcl (and mac-osx cmu)) (not linuxppc-target)) windoze) "ld")
(defvar *cc* #+windoze "cl" #-windoze "gcc")
(defvar *ldflags* #+windoze " " #-windoze " -r -o ")

;;;
;;; What was here in all.lisp went to compile-and-load.lisp
;;;

;;; --------------------------------
;;; compile the .c files and build libraries, where applicable

(defvar someone-compiled nil)
(setf someone-compiled nil)
(defvar someone-loaded nil)
(setf someone-loaded nil)

(defvar c-compiler "cc") ; shouldn't this be *cc*? 

(defun cc-it (n) 
  (let* ((lname (concatenate 'string clm-bin-directory n *obj-ext*))
	 (cname (concatenate 'string clm-directory n ".c"))
	 (cc-name (or #+(and excl (not windoze)) (sys:getenv "CC")
		      #+lucid (lcl:environment-variable "CC")
		      #+cmu (cdr (assoc (intern "CC" "KEYWORD") ext:*environment-list*))
		      #+sbcl "gcc"
		      #+clisp "gcc"
		      #+lispworks "gcc"
		      #-(or sun hpux windoze) "cc"
		      #+(or sun hpux) "gcc"
		      #+windoze "cl"
		      "gcc"))
	 (cstr (concatenate 'string 
			    cc-name " "
			    cname
			    " -c"
			    *cflags*
			    #-windoze " -o " #+windoze " -Fo"
			    lname
			    )))
    (setf c-compiler cc-name)
    (when (or (not (probe-file lname))
	      (not (probe-file cname))
	      (> (file-write-date (truename cname)) (file-write-date (truename lname))))
      (princ (format nil "; Compiling ~S~%" cname)) (force-output)
      #+debug (princ (format nil "; ~A~%" cstr)) (force-output)
      (setf someone-compiled t)
      #+excl (excl:shell cstr)
      #+lispworks (sys::run-shell-command cstr)
      #+clisp (ext:shell cstr)
      #+sbcl (sb-ext:run-program "/bin/csh" (list "-fc" cstr) :output t)
      #+cmu (extensions:run-program "/bin/csh" (list "-fc" cstr) :output t)
      #+openmcl (ccl:run-program "/bin/csh" (list "-fc" cstr) :output t)
      )))

(cc-it "io")
(cc-it "headers")
(cc-it "audio")
(cc-it "sound")
(cc-it "clm")
(cc-it "cmus")

#+(or cmu sbcl excl openmcl clisp lispworks)
(let ((shared-name (concatenate 'string clm-bin-directory "libclm." *shared-object-extension*)))
  (when (or someone-compiled (not (probe-file shared-name)))
    (princ (format nil "; Creating ~S~%" shared-name))
    (setf someone-loaded t)
    (let ((str (concatenate 
		'string
		*ld* " "
		#-(and cmu freebsd) *csoflags* " "
		#+(and cmu freebsd) "-r -L/usr/lib "
		#+(or (and clisp mac-osx) (and sbcl darwin) (and openmcl (not linux-target) (not linuxppc-target))) "-dynamiclib "
		#-windoze "-o " #+windoze "-Fe"
		shared-name " "
		clm-bin-directory "headers" *obj-ext* " "
		clm-bin-directory "audio" *obj-ext* " "
		clm-bin-directory "io" *obj-ext* " "
		clm-bin-directory "sound" *obj-ext* " "
		clm-bin-directory "clm" *obj-ext* " "
		clm-bin-directory "cmus" *obj-ext* " "
		#+windoze clm-bin-directory #+windoze "libclm.def "

		#+sgi "-laudio "
		#+alsa "-lasound "
		#+jack "-ljack -lsamplerate -lasound "
		;#+sb-thread "-lpthread "

		;; try to find the ACL shared library that has acl_printf and call_lisp_address
		;#+(and sgi allegro-v5.0) (concatenate 'string (namestring (truename "sys:")) "libacl5.0.so ")
		
		;#+(and (or allegro-v5.0 allegro-v5.0.1 allegro-v6.0) (not (or windoze linux)))
		#+(and acl-50 (or sun (not acl-70)) (not (or windoze linux)))
		(concatenate 'string (namestring (translate-logical-pathname "sys:")) (excl::get-shared-library-name) " ")

		;; cl-lite version of get-shared-library-name returns a name that doesn't exist
		;;   and we can't include it anyway because of appalling Windoze file name stupidity
		;;   windoze users are on their own here -- fixup the &#@%$^# name below.
		#+(and windoze (not allegro-cl-lite) (not acl-62) (not acl-70)) clm-bin-directory
		#+(and windoze (not allegro-cl-lite) (not acl-60) (not acl-61) (not acl-62)) "acl503.lib "
		#+(and windoze (not allegro-cl-lite) acl-60 (not acl-61) (not acl-62)) "acl601.lib "
		#+(and windoze (not allegro-cl-lite) acl-62 (not acl-70)) (concatenate 'string "\"" (namestring (truename "sys:")) "acli623.lib\"")
		#+(and windoze (not allegro-cl-lite) acl-70 (not acl-80)) (concatenate 'string "\"" (namestring (truename "sys:")) "acl701.lib\"")

                #+(and windoze (not allegro-cl-professional) acl-80) (concatenate 'string "\"" (namestring (truename "sys:")) "acli8010t.lib\"") 
                #+(and windoze allegro-cl-professional acl-80) (concatenate 'string "\"" (namestring (truename "sys:")) "acli8010.lib\"") 
		
		#+windoze " user32.lib gdi32.lib kernel32.lib comctl32.lib comdlg32.lib winmm.lib advapi32.lib "
		
		#-(or windoze (and openmcl linuxppc-target) (and linux (or cmu sbcl acl-50))) " -lm -lc"
		;;; in linux this -lc can cause "ld: internal error ldlang.c 3088"
		#+(or mac-osx os-macosx) " -framework CoreAudio"
		#+(and sun (not x86-64)) " -lgcc"
		#+(and clozure darwinx8632-target) " -arch i386"
		)))
      (format t ";;~A~%" str)
      (force-output)
      #+excl (excl:shell str)
      #+cmu (extensions:run-program "/bin/csh" (list "-fc" str) :output t)
      #+openmcl (ccl:run-program "/bin/csh" (list "-fc" str) :output t)
      #+sbcl (sb-ext:run-program "/bin/csh" (list "-fc" str) :output t)
      #+clisp (ext::shell str)
      #+lispworks (sys::run-shell-command str)

      (if (not (probe-file shared-name))
	  (format t "~S was not created?  Perhaps there was a C compiler or loader error.~%~
                     You might try the following command in a terminal to see what happened: ~S~%"
		  shared-name
		  str))
      ;; gcc creates a library file even if some error occurs!  So, this check won't usually help.
      )))

    

