#lang setup/infotab

(define name "sr")
(define scribblings '(("doc/manual.scrbl" ())))

(define blurb '("Program to track and rate series you consume."))
(define primary-file "main.rkt")

(define required-core-version "6.0")

(define deps '("base" "scribble-lib"))
(define build-deps '("gui-lib" "racket-doc"))
