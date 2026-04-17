;;;; setup
;; conda create -n my-env
;; conda install sbcl ...

;; symlink local asd files
;;   ln -s $(realpath my.system.asd) ~/common-lisp/

;;;; startup
;; M-x conda-env-activate vcf-report
;; , ' start repl

;;;; register all
;; setwd to project
;; (uiop:chdir #P"/home/user/")
;; refresh asdf registry
;;   (asdf:initialize-source-registry
;;    (list :source-registry
;;          (list :directory (uiop:getcwd))
;;          (list :tree (uiop:native-namestring "~/common-lisp/"))
;;          :inherit-configuration))

;; set ocicl to reproducible
;;   (setf ocicl-runtime:*local-only* t)

;; load
;;   (asdf:load-system :lamb.files.vcf)

;; clone repos if Component :SYSTEM not found in ocicl
;;   git clone https://github.com/user/repo.git ~/common-lisp/repo
