(in-package #:cl-user)
(defpackage #:vcf-rename
  (:use #:cl)
  (:local-nicknames (:c :cmd)
                    (:i :iterate))
  (:export :check-dependencies
           :head
           :positions
           :stats
           :lines
           :mimic-rename
           :dash-int-dot=
           :missing-from
           :missing-in-range
           :filter-and-sanitize-map
           :rename-vcf
           ))

(in-package :vcf-ops)

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
  (c:$cmd (format nil "bcftools head ~A" vcf)))

(defun stats (vcf)
  (c:$cmd (format nil "bcftools stats ~A" vcf)))

(defun lines(vcf)
  "takes a #P to a vcf file and returns all line-names"
  (let* ((str (c:$cmd (format nil "bcftools query -l ~A" vcf)))
         (separated (str:lines str)))
    separated))

;; renaming
(defun mimic-rename (map &rest edits)
  "
ARGS:
map: a-list mapping encoded-names (ie our encoding) to entry-names
edits: series of small functions which  take and return 1 string
DOES:
compose the change functions
executes functions l->r in order to mimic the lab rename
modify the our encoding to match vcf encoding
RETS:
alist from encoded-names-modified (ie vcf) to entry-names
"
  (let (
        (composed-edits
          (apply #'alexandria:compose (reverse edits)))
        )
    (flet (
           (mod-alist (pair)
             (cons (funcall composed-edits (car pair))
                   (cdr pair)))
           )
      (mapcar #'mod-alist map)
      )))

(defun dash-int-dot= (str1 str2)
  "
ARGS:
str1: string like ALPHanum3r1c_sample-9999.alph
str2: string like ALPHanum3r1c_sample-9999.alph
DOES:
drop all from ^ to the last _ or -
drop all from first . to $
tests an int is found
compares integers for equality
RETS:
T if int1=int2
"
  (labels (
           (drop-head (str)
             (str:replace-first "^.*[-,_]+" "" str :regex t))
           (drop-tail (str)
             (str:replace-first "[.].*$" "" str :regex t))
           )
    (let* (
           (both (list str1 str2))
           (noheads (mapcar #'drop-head both))
           (notails (mapcar #'drop-tail noheads))

           (same (= (parse-integer (second notails))
                  (parse-integer (first notails))))
           )
      same)))

(defun missing-from (vcf-names map-vcf-entry &key (test #'string=) which)
  "
ARGS:
vcf-names: list of lines in vcf file
map-vcf-entry: alist mapping vcflines to entry-names
which: one of :vcf :pof
test: a function #'string= or a custom test to find ints in some stupid string
DOES:
uses the plate organizer data to check which lines are missing from one arg
we expect
some missing from vcf (filtered samples and/or multiple plates)
none missing from pof (maybe wrong plate organizer file)
RETS:
unmatched items remaining in the non :which input
as alist (pof remaining) or list (vcf remaining)

vcf   pof
aaa    (aaa . name) matches returns nothing
x     (aaa . name) missing from :vcf returns (aaa . name)
aaa     x          missing from :pof returns aaa
"
  (assert (or (equal which :vcf)
              (equal which :pof))
          (which)
          "the keyword :which must be one of :vcf or :pof")
  (let (
         ;; make list into alist for symmetric compatibility
         (vcf-alist (mapcar
                     #'(lambda (i) (cons i "name-unknown"))
                     vcf-names
                     )))
    (if (equal which :vcf)
        ;; vcf
        (set-difference map-vcf-entry vcf-alist :key #'car :test test)
        ;; pof
        (mapcar #'car (set-difference vcf-alist map-vcf-entry :key #'car :test test))
        )))

(defun missing-in-range (map-vcf-entry-missing start n &rest edits)
  "
ARGS:
map-vcf-entry-missing: alist of results from missing-in-range
start: int to begin with eg 1
n: int expected number, including start
edits: a series of small functions which will take the car of first arg and remove all non numeric chars
DOES:
operates on cars of first arg
edit to be numeric
exclude any which is outside the range
RETS:
alist in correct range
"
  (labels (
           (make-car-numeric (pair)
             (let* (;; wraps and unwraps to reuse mimic-rename
                    (alist-pair (list pair)) ;; wrap
                    (alist-numcar
                      (apply #'mimic-rename alist-pair edits)) ;; apply to unlist edits
                    (pair-numcar (car alist-numcar)) ;; unwrap
                    (number (parse-integer(car pair-numcar))))
               (if (and (> number 0)
                        (integerp number))
                   number ;; return this integer
                   (error "edits did not yield a positive integer. currently: ~S " number))))
           (in-range (int)
             ;;  start int last in that order
             (<= start int (+ start (1- n))))
           )
      (remove-if-not #'in-range map-vcf-entry-missing :key #'make-car-numeric)
      ))

;; renaming functions
(defun modify-values (alst fun)
  "
ARGS:
alst: alist with key, value
fun: #'a-function
DOES:
modifies values with fun
RETS:
alist with key, modified-value
"
  (mapcar #'(lambda (kv) (cons (car kv)
                               (funcall fun (cdr kv))))
          alst))



(defun drop-if-not-named (names alst test)
  "
ARGS:
names: list of strings
alst: alist of elements (STRING . something)
DOES:
drops from alst when car is not in names by string=
RETS:
alst without non matches
"
  (let* (
         ;;make symmetrical
         (kk (mapcar #'(lambda (k) (cons k k))
                     names))
         (named (intersection alst
                              kk
                              :key #'car :test test))
         )
    (reverse named)))

(defun map-reheader-file (map path)
  "
convert map alist into a file
you must know that order and length and identity of cdr of  map are correct

newname
newname1

"
  (with-open-file (out path :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)

    (mapcar #'(lambda (kv)
                (format out "~&~A~%" (cdr kv)))
            map))
  path)

(defun map-matches-vcfp (vcf-file proposed-map-vcf-entry &key (test #'string=))
  "
ARGS:
vcf-file: #P to vcf file
proposed-map-vcf-entry: prepared alist of lines to be renamed
DOES:
extracts lines from vcf as is
tests if proposed lines (car of alist) are same (order and identities)
RETS:
T or nil
"
  (let* (
        (lines (vcf-ops:lines vcf-file))
        (proposed-lines (mapcar #'(lambda (kv) (car kv))
                                proposed-map-vcf-entry))
        (same-length (= (length lines)
                        (length proposed-lines)))
        (same-contents (mapcar #'(lambda (a b) (funcall test a b))
                        lines proposed-lines))
        (all-pass (every #'identity same-contents))
        )
    (if (and same-length all-pass)
        T
        nil)))

(defun bcftools-reheader-rename (vcf-in vcf-out names-file)
  "
renames a vcf header using manifest file
warning no safety
use map-matches-vcfp first or use rename-vcf
"
  (cmd:$cmd (format nil "bcftools reheader --samples ~A -o ~A ~A" names-file vcf-out vcf-in)))

(defun match-order (list map &key (test #'string=))
  "
ARGS:
list of items
alist of pairs
test two item comparison for item of list and first item of map
DOES:
use list to test first items of map, reordering map to match
very efficient if both list and map are sorted and match
RETS:
reordered and filtered map
"
  (let (
        (source (copy-tree map)) ; deep copy map
        (target '()) ; initally empty
        )
    ;; loop over list
    (iter:iter (iter:for i in list)
      ;; search first item of map
      (let ((found (find i source :key #'first :test test)))
        (when found
          (push found target) ;; add
          (remove found source :test #'equal) ;; remove
          )))
    ;; return found items in original order
    (reverse target)
    ))


(defun rename-vcf (in-vcf-file proposed-map-vcf-entry &key (test #'string=))
  "
ARGS:
in-vcf-file: #P to vcf to rename
proposed-map-vcf-entry: alist of (current-name . re-name)
DOES:
test if vcf linenames and proposed linenames are matching
produces temporary reheader formatted file
calls bcftools reheader
creates a file like in-vcf
RETS:
path to renamed vcf file
"
  (let (
        (out-vcf-file (make-pathname :defaults in-vcf-file
                                     :name (concatenate 'string (pathname-name in-vcf-file) "_renamed")))
        (reordered (match-order (lines in-vcf-file) proposed-map-vcf-entry :test test))
        ;; known order is correct

        )

    ;; test before proceeding
    (assert (map-matches-vcfp in-vcf-file reordered :test test) ()
            "the vcf file and proposed renaming map do not match")
    ;; known length is correct
    ;; known all match by test
    ;; temp reheader file
    (uiop:with-temporary-file (:pathname temp)
      (map-reheader-file reordered temp)
      ;; reheader
      (bcftools-reheader-rename in-vcf-file out-vcf-file temp))
    out-vcf-file))

(defun filter-and-sanitize-map (vcf-names map-vcf-entry &key (test #'string=))
  (modify-values
   (print
    (drop-if-not-named vcf-names map-vcf-entry test))
   #'string-clean:sanitize-string))
