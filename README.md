# CLM

*********************************************************************
The original CLM from https://ccrma.stanford.edu/software/snd/snd/clm.html is now loadable with ASDF.
Simply put it in your local directory and (ql:quickload "clm")
*********************************************************************



Once loaded you can do:

(load "~/quicklisp/local-projects/clm/bird.clm")

and listen to the birds...

or something like:

(compile-and-load "v")

(with-sound (:channels 2)
	   (fm-violin 0 5 280 0.1 :degree 45))

evaluate (play) or (dac) to play the latest sound
and (stop-play) to stop it.

