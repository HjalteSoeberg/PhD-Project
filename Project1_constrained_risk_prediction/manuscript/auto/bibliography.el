;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "bibliography"
 (lambda ()
   (LaTeX-add-bibitems
    "gerds2021medical"
    "super"))
 '(or :bibtex :latex))

