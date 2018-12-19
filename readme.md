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

## Day 1

* [day01.clj](day01/day01.clj)
* [day01.lisp](day01/day01.lisp)
* [day01-streams.lisp](day01/day01-streams.lisp)
* `awk { sum += $1 } END { print sum }`


## Day2

* [day02.clj](day02/day02.clj)


## Notes on clojure

I keep writing `defun` instead of `defn`.
I keep using parenthesis instead of square brackets.

