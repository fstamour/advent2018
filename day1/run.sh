#!/bin/sh

time sbcl --noinform --script day1.lisp
time sbcl --noinform --script streams.lisp
time clojure day1.clj
