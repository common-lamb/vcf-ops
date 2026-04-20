(in-package #:cl-user)
(defpackage #:vcf-filter
  (:use #:cl)
  (:export :check-dependencies
           :keep-samples
           :hapmap-keep-samples
           ))

(in-package :vcf-filter)

(defun check-dependencies ()
  "
&&& make restartable
"
  ;;(assert (not (null )))
  (cmd:$cmd "which bcftools"))

(defun keep-samples (infile outfile samples)
  "
&&& merge
"
  (let (
        (infile-str (namestring infile))
        (outfile-str (namestring outfile))
        (comma-sep-list (str:join "," samples))
        )

    (tmpdir:with-tmpdir (tmp)
      (let* ((infile-tmp (make-pathname :defaults infile
                                        :directory (pathname-directory tmp)))
            (outfile-tmp (make-pathname :defaults outfile
                                       :directory (pathname-directory tmp)))
            (infile-gz (make-pathname :defaults infile-tmp
                                      :name (concatenate 'string
                                                         (pathname-name infile-tmp)
                                                         ".vcf")
                                      :type "gz")))
        (uiop:copy-file infile infile-tmp)
        (print "copied")
        (print (uiop:directory-files tmp))

        (cmd:cmd (format nil "bgzip --threads 8 '~A'" infile-tmp))
        (print "compressed")
        (print (uiop:directory-files tmp))

        (cmd:cmd (format nil "tabix '~A'" infile-gz))
        (print "indexed")
        (print (uiop:directory-files tmp))

        (cmd:cmd
         (format nil "bcftools view --samples '~A' --output '~A' '~A'"
                 comma-sep-list
                 outfile-tmp
                 infile-gz))
        (print (uiop:directory-files tmp))
        ))))


(defun vcf-subset-by-samples (vcf samples &key (output-dir *process-dir*))
  "

&&& merge
ARGS:
DOES:
RETS:
"
  ;;create file name
  (let* ((out-file (make-pathname :defaults output-dir
                                :name (concatenate 'string
                                                   (pathname-name vcf)
                                                   "_subset-by-samples")))
         (zip-file (make-pathname :defaults out-file
                                  :type "bgz"))
         (tabix-file (make-pathname :defaults out-file
                                  :type "tbx"))

       ;; interpose commas to samples
       (sample-arg (str:join "," samples))
         )
    ;; filter

    (cmd:$cmd (format nil "bgzip -o '~A' '~A'"
                      (namestring zip-file)
                      (namestring vcf)))
    (cmd:$cmd (format nil "tabix -p vcf -csi --zero-based --force '~A'"
                      (namestring zip-file)))
    (cmd:$cmd (format nil "bcftools view --force-samples -s '~A' '~A' > '~A'"
                      sample-arg
                      (namestring vcf)
                      (namestring out-file)))








    ;; return file name
    out-file))

(defun hapmap-keep-samples (hapmap-file samples &key (output-dir *process-dir*))
  "
ARGS:
hapmap-file: path to hapmap file
samples: list of strings being entry-names
DOES:
uses tassel to include only entry-names in samples list
RETS:

&&& move to hapmap-ops
"
  ;; create file name
  (let ((output-file (make-pathname :defaults output-dir
                                    :name (pathname-name hapmap-file)
                                    :type nil))
        ;; interpose commas to samples
       (sample-arg (str:join "," samples)))
    ;; subset it
    (cmd:cmd (format nil "run_pipeline.pl -fork1 -h '~A' -includeTaxa '~A' -export '~A' -exportType Hapmap -runfork1"
                      (namestring hapmap-file)
                      sample-arg
                      (namestring output-file)))
    ;; return file name
    output-file))
