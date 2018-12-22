# Advent of code 2018

https://adventofcode.com/2018

## Dependencies

* `sbcl` or any common lisp implementation
* `clojure` (haven't tried clojurescript)
* `boot`

## Notes

```shell
nix-shell --pure -p boot --run 'boot repl'
```

## Day 1 - Chronal calibration

* [day01.clj](day01/day01.clj)
* [day01.lisp](day01/day01.lisp)
* [day01-streams.lisp](day01/day01-streams.lisp)
* `awk { sum += $1 } END { print sum }`


## Day2 - Inventory management system

* [day02.clj](day02/day02.clj)

## Day3 - No matter how you slice it

* [day03.clj](day03/day03.clj)


## Notes on clojure

* I keep writing `defun` instead of `defn`.
* I keep using parenthesis instead of square brackets.
* It seems overly-complicated to write code that is portable between clojure and
  clojurescript (e.g. `js/parseInt` v.s. `Integer/parseInt` or
  `clojure.java.io/reader`)
* There's no equivalent to `#+nil`, the closest is `(comment ...)` but it still
  part of the syntax tree.
* That one time I forgot that `for` is lazy, when trying to mutate a Java array...
* I got a few java.NullPointerException without any traces, ugh...

