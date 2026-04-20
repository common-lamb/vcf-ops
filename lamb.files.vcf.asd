(defsystem "lamb.files.vcf"
  :description "vcf operations: conversion info filtering rename"
  :author "common-lamb (https://github.com/common-lamb)"
  :version "0.0.1"
  :license "MIT"
  :depends-on (
               ;; clone first
               :filepaths ; git clone https://github.com/fosskers/filepaths.git ~/common-lisp/filepaths

               ;; essential
               :cmd
               :str
               :alexandria
               :serapeum
               :iterate
               :bordeaux-threads

               ;; this project
               :filesystem-utils
               :tmpdir
               :file-finder
               :lamb.data.entry-name
               )
  :serial t
  :components ((:file "vcf-convert")
               (:file "vcf-filter")
               (:file "vcf-info")
               (:file "vcf-rename"))
  )
