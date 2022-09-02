;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'interis)
(require 'buttercup)

(require 'faceup)

(describe "Propertizing text"
  (it "sets read-only"
    (let ((faceup-properties '(read-only face))
          (faceup-test-explain t))
      (expect
       (faceup-test-equal
        (faceup-markup-string
         (interis--propertize-read-only "Hello!" t t
           'face 'bold))
        "«B:«(read-only):t:Hello!»»")))))
