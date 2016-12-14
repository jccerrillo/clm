# CLM
A Quicklisp-loadable version of [CLM (Common Lisp Music)](https://ccrma.stanford.edu/software/snd/snd/clm.html).

Tested with ClozureCL, SBCL and Lispworks (Personal Edition) on OSX.

Installation:

1) Clone or download to ~/quicklisp/local-projects directory.

2) Start your lisp

3) (ql:register-local-projects)

4) (ql:quickload "clm")


Once loaded you can do:

(load "~/quicklisp/local-projects/clm/bird.clm")

and listen to the birds...

or something like:

(compile-and-load "v")

(with-sound (:channels 2)
	   (fm-violin 0 5 280 0.1 :degree 45))

evaluate (play) or (dac) to play the latest sound
and (stop-play) to stop it.

If you want to load the system on a different lisp, it is easiest to delete everything and install a fresh version.