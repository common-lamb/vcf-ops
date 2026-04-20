(in-package #:cl-user)
(defpackage #:vcf-info
  (:use #:cl)
  (:export :check-dependencies
           :head
           :positions
           :stats
           :lines
           ))

(in-package :vcf-info)

(defun check-dependencies ()
  "
&&& make restartable
"
  ;;(assert (not (null )))
  (c:$cmd "which bcftools"))

(defun positions (vcf)
  "
ARGS:
vcf: a #P to a vcf file
DOES:
we suppress the error 'not in header' as we are only interested in positions
formats each position as: \"chr pos\"
RETS:
list of all variant positions

"
  (let* ((str (uiop:run-program
               (format nil "bcftools query -f '%CHROM %POS\\n' ~A 2>/dev/null" vcf )
               :output :string))
         (separated (str:lines str)))
    separated))

(defun head (vcf)
  (cmd:$cmd (format nil "bcftools head ~A" vcf)))

(defun stats (vcf)
  (cmd:$cmd (format nil "bcftools stats ~A" vcf)))

(defun lines(vcf)
  "takes a #P to a vcf file and returns all line-names"
  (let* ((str (cmd:$cmd (format nil "bcftools query -l ~A" vcf)))
         (separated (str:lines str)))
    separated))
