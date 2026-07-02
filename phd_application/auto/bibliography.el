;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "bibliography"
 (lambda ()
   (LaTeX-add-bibitems
    "delphi"
    "gerds2021medical"
    "guo2023"
    "isaksen2023validation"
    "li2016"
    "liu2024predicting"
    "parikh2021"
    "super"
    "vistisen2016prediction"
    "wani2024"
    "wright2021"
    "SDmapSubgroup"
    "subgrouptree"
    "datadrivensubgroup"
    "dynamicdeephit"
    "attention"))
 '(or :bibtex :latex))

