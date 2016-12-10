(defpackage #:clm 
  (:use #:cl)
  (:nicknames :clm)
  (:import-from :walker walk-form)
  (:import-from :cl-user compile-and-load)
  (:import-from :ccl quit %get-cstring))
