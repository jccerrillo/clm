#+quicklisp
(asdf:defsystem #:clm
  :name "clm"
  :version "5"
  :description "Common Lisp Music for Quicklisp"
  :author "William Schottstaedt <bil (at) ccmra (dot) stanford (dot) edu>"
  :licence "LLGPL"
  :serial t
  :components ((:file "dir-setup")
	       (:file "comp-load")
	       #-(or excl sbcl)(:file "walk")
	       (:file "package")
	       (:file "all1" )
	       (:file "initmus")
	       (:file "all2")
	       #+ccl(:file "mcl-doubles")
	       (:file "sndlib2clm")
	       (:file "defaults")
	       (:file "ffi")
	       (:file "mus")
	       (:file "run")
	       (:file "sound")
	       (:file "defins")
	       (:file "env")
	       (:file "export")
	       (:file "clm1")
	       (:file "play-stop-sound")
	       (:file "all-end")))

#-quicklisp
(asdf:defsystem "clm"
  :description "Common Lisp Music"
  :version "3"
  :author "William Schottstaedt <bil (at) ccmra (dot) stanford (dot) edu>"
  :licence "LLGPL"
  :components ((:file "all" )))
