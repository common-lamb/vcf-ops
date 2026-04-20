(in-package #:cl-user)
(defpackage #:vcf-convert
  (:use #:cl)
  (:export
   :hapmap->vcf
   ))

(in-package :vcf-convert)

(defun hapmap->vcf (hapmap-file &key (output-dir *process-dir*))
  "
ARGS:
hapmap-file: path to hapmap
DOES:
uses tassel to convert
RETS:
path to vcf
"
  ;; create file name
  (let ((output-file (make-pathname :defaults output-dir
                                    :name (pathname-name hapmap-file)
                                    :type "vcf")))
    ;; convert it
    (cmd:cmd (format nil "run_pipeline.pl -fork1 -h '~A' -export '~A' -exportType VCF -runfork1"
                      hapmap-file
                      output-file))
    ;; return file name
    output-file))
