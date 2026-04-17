(defsystem "lamb.files.vcf"
  :description "vcf operations, summary report"
  :author "common-lamb (https://github.com/common-lamb)"
  :version "0.0.1"
  :license "MIT"
  :depends-on (
               ;; clone
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
               :file-finder
               :lamb.data.clean
               )
  :serial t
  :components ((:file "vcf-ops"))
  )
