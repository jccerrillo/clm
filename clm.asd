;;; ASDF system definition file for CLM
;;; For information on ASDF see: http://www.cliki.net/asdf
;;;
;;; To load CLM from a non-standard install location:
;;;
;;; (require :asdf)
;;; (push "/path/to/clm-3/" asdf:*central-registry*)
;;; (asdf:operate 'asdf:load-source-op :clm)
;;;
;;; To download/install/load CLM from its archive:
;;;
;;; (require :asdf)
;;; (progn (push "/path/to/asdf-install/" asdf:*central-registry*)
;;;        (asdf:operate 'asdf:load-op 'asdf-install))
;;; (asdf-install:install 'clm)
;;; (asdf:operate 'asdf:load-source-op 'clm)

(asdf:defsystem #:clm
  :name "clm"
  :version "5"
  :description "Common Lisp Music for Quicklisp"
  :author "William Schottstaedt <bil (at) ccmra (dot) stanford (dot) edu>"
  :licence "LLGPL"
  :serial t
  :components ((:file "dir-setup")
	       (:file "comp-load")
	       (:file "walk")
	       (:file "package")
	       (:file "all1" )
	       (:file "initmus")
	       (:file "all2")
	       (:file "mcl-doubles")
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

