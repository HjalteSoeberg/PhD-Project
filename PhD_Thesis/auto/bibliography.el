;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "bibliography"
 (lambda ()
   (LaTeX-add-bibitems
    "delphi"
    "gerds2021medical"))
 '(or :bibtex :latex))

