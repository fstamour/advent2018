#!/bin/sh

time sbcl --noinform --script day01.lisp
time sbcl --noinform --script day01-streams.lisp
time clojure day01.clj
