#|

DESC: tools/ffi-gen.lisp - code that reads defs and generates actual ffi-code.

|#

(in-package :org.langband.ffi)

(defstruct foreign-fun
  c-name
  lisp-name
  returns
  arguments
  only-when)

(defstruct foreign-type
  old-name
  new-name)

(defconstant +result-package+ :org.langband.ffi)
(defconstant +export-functions+ t)
(defconstant +export-types+ t)

(defvar *foreign-functions* nil)
(defvar *foreign-types* nil)
(defvar *names-to-export* nil)

(defun ensure-foreign-function (names &key (returns :int) (args nil) (only-when nil))
  "Defines a foreign type."
  (unless (and (consp names) (stringp (car names)))
    (warn "Want a (STRING LISP-VAL) as name for a function.")
    (return-from ensure-foreign-function nil))

  (let ((lisp-name (cadr names)))
    (push (make-foreign-fun :c-name (car names)
			    :lisp-name lisp-name
			    :returns returns
			    :arguments args
			    :only-when only-when)
	  *foreign-functions*)
    lisp-name))

(defmacro def-foreign-function (names &rest the-rest)
  `(ensure-foreign-function ',names ,@the-rest))

(defun ensure-foreign-type (new-name old-name)
  "Defines a foreign type."
  (push (make-foreign-type :old-name old-name :new-name new-name)
	*foreign-types*)
  new-name)

(defmacro def-foreign-type (new-name old-name)
  `(ensure-foreign-type ',new-name ',old-name))

(defgeneric generate-foreign-functions (foreign-list stream backend)
  (:documentation "generates foreign functions to a backend and writes code to stream."))

(defgeneric generate-foreign-types (foreign-list stream backend)
  (:documentation "generates foreign types to a backend and writes code to stream."))

(defgeneric generate-header (stream backend)
  (:documentation "Writes header to the stream."))

(defgeneric generate-footer (stream backend)
  (:documentation "Writes footer to the stream."))

(defgeneric get-arg-info (arg backend)
  (:documentation "Returns an appropriate argument info obj."))

(defun print-restrict (when-restrict stream)
  "Prints reader restriction to stream."
  (when when-restrict
    (format stream "~2&#+~a~%" when-restrict)))

(defun export-name (name stream)
  "Writes export statement to stream."
  (pprint `(export ',name)
	  stream))

;;; ====

(defmethod get-arg-info (arg (backend (eql :cmucl)))
  (let ((the-type (get-type-translation (car arg) backend))
	(the-name (cadr arg)))
    (case the-type
      ((cptr byte angbyte char) (list the-name the-type))
      (otherwise (list the-name the-type :in)))))

(defmethod get-arg-info (arg (backend (eql :sbcl)))
  (let ((the-type (get-type-translation (car arg) backend))
	(the-name (cadr arg)))
    (case the-type
      ((cptr byte angbyte char) (list the-name the-type))
      (otherwise (list the-name the-type :in)))))

(defmethod get-arg-info (arg (backend (eql :acl)))
  (let ((the-name (cadr arg)))
    (multiple-value-bind (the-type the-passed-val)
	(get-type-translation (car arg) backend)

      (cond ((and the-type the-passed-val)
	     (list the-name the-type the-passed-val))
	    (the-type
	     (list the-name the-type))
	    (t
	     (list the-name))))))

(defmethod get-arg-info (arg (backend (eql :clisp)))
  (let ((the-type (get-type-translation (car arg) backend))
	(the-name (cadr arg)))
    (if the-type
	(list the-name the-type)
	(list the-name))))

(defmethod get-arg-info (arg (backend (eql :lispworks)))
  (let ((the-type (get-type-translation (car arg) backend))
	(the-name (cadr arg)))
    (if the-type
	(list the-name the-type)
	(list the-name))))

(defmethod get-arg-info (arg (backend (eql :openmcl)))
  (let ((the-type (get-type-translation (car arg) backend))
	(the-name (cadr arg)))
    (if the-type
	(list the-name the-type)
	(list the-name))))

;;; ====

(defmethod generate-foreign-functions (foreign-list stream (backend (eql :cmucl)))
;;  (warn "generating functions for cmu..")
  
  (dolist (i foreign-list)
    (let ((c-name (foreign-fun-c-name i))
	  (lisp-name (foreign-fun-lisp-name i))
	  (when-restrict (foreign-fun-only-when i)))


      (print-restrict when-restrict stream)
      (pprint `(declaim (inline ,lisp-name))
	      stream)
      
      (print-restrict when-restrict stream)

      (format stream "~&(alien:def-alien-routine (~s ~s)~%           ~s~{~&           ~s~})~%"
	      c-name lisp-name (get-type-translation (foreign-fun-returns i) backend)
	      (mapcar #'(lambda (x) (get-arg-info x backend)) (foreign-fun-arguments i)))

      #||
      (when +export-functions+
	(print-restrict when-restrict stream)
	(export-name lisp-name stream))
      ||#
      
      (terpri stream)
      
      )))

(defmethod generate-foreign-functions (foreign-list stream (backend (eql :sbcl)))
;;  (warn "generating functions for cmu..")
  
  (dolist (i foreign-list)
    (let ((c-name (foreign-fun-c-name i))
	  (lisp-name (foreign-fun-lisp-name i))
	  (when-restrict (foreign-fun-only-when i)))


      (print-restrict when-restrict stream)
      (pprint `(declaim (inline ,lisp-name))
	      stream)
      
      (print-restrict when-restrict stream)

      (format stream "~&(sb-alien:define-alien-routine (~s ~s)~%           ~s~{~&           ~s~})~%"
	      c-name lisp-name (get-type-translation (foreign-fun-returns i) backend)
	      (mapcar #'(lambda (x) (get-arg-info x backend)) (foreign-fun-arguments i)))

      #||
      (when +export-functions+
	(print-restrict when-restrict stream)
	(export-name lisp-name stream))
      ||#
      
      (terpri stream)
      
      )))


(defmethod generate-foreign-functions (foreign-list stream (backend (eql :acl)))

  (dolist (i foreign-list)
    (let ((c-name (foreign-fun-c-name i))
	  (lisp-name (foreign-fun-lisp-name i))
	  (when-restrict (foreign-fun-only-when i)))

      (print-restrict when-restrict stream)

      (let ((args (mapcar #'(lambda (x) (get-arg-info x backend)) (foreign-fun-arguments i)))
	    (the-retval (get-type-translation (foreign-fun-returns i) backend)))
	(format stream "~&(ff:def-foreign-call (~a ~s) ~s :returning ~s)~%"
		lisp-name c-name args the-retval))

      #||
      (when +export-functions+
	(print-restrict when-restrict stream)
	(export-name lisp-name stream))
      ||#
      
      (terpri stream)
      )))

(defmethod generate-foreign-functions (foreign-list stream (backend (eql :openmcl)))

  (dolist (i foreign-list)
    (let ((c-name (foreign-fun-c-name i))
	  (lisp-name (foreign-fun-lisp-name i))
	  (when-restrict (foreign-fun-only-when i)))

      (print-restrict when-restrict stream)

      (let* ((args (mapcar #'(lambda (x) (get-arg-info x backend)) (foreign-fun-arguments i)))
	     (arg-names (mapcar #'car args))
	     (the-retval (get-type-translation (foreign-fun-returns i) backend))
	     (nest-level 0)
	     (new-args '()))
	(format stream "~&(defun ~a ~s~%" lisp-name arg-names)
	(dolist (i args)
	  (cond ((eq (second i) 'cptr)
		 (let ((sym (gensym (symbol-name (first i)))))
		   (format stream "~& (ccl::with-cstr (~a ~s)~%" sym (first i))
		   (incf nest-level)
		   (push (list sym :address) new-args)))
		(t
		 (push (list (first i) (second i)) new-args))))
	
	(setf args (nreverse new-args))
		 
	(format stream "~&  (ccl::external-call \"_~a\"" c-name)

	(when args
	  (dolist (i args)
	    (format stream " ~s ~a" (second i) (first i))))
	
	(format stream " ~s)" the-retval)
	(dotimes (i nest-level)
	  (format stream ")"))
	(format stream ")~%")
	)

      (terpri stream)
      )))

(defmethod generate-foreign-functions (foreign-list stream (backend (eql :lispworks)))
  
  (dolist (i foreign-list)
    (let ((c-name (foreign-fun-c-name i))
	  (lisp-name (foreign-fun-lisp-name i))
	  (the-retval (get-type-translation (foreign-fun-returns i) backend))
	  (args (mapcar #'(lambda (x) (get-arg-info x backend)) (foreign-fun-arguments i)))
	  (when-restrict (foreign-fun-only-when i)))
      (print-restrict when-restrict stream)
      
      (format stream "~&(fli:define-foreign-function (~a ~s) ~s~%   :result-type ~s :language :c :calling-convention :stdcall :module :lang-ffi)~%"
	      lisp-name c-name args
	      the-retval)
      (terpri stream)
      )))

(defmethod generate-foreign-functions (foreign-list stream (backend (eql :clisp)))

  (dolist (i foreign-list)
    (let ((c-name (foreign-fun-c-name i))
	  (lisp-name (foreign-fun-lisp-name i))
	  (when-restrict (foreign-fun-only-when i)))

      (print-restrict when-restrict stream)

      (let ((args (mapcar #'(lambda (x) (get-arg-info x backend)) (foreign-fun-arguments i)))
	    (the-retval (get-type-translation (foreign-fun-returns i) backend)))
	(format stream "~&(ffi:def-call-out ~a (:name ~s) (:language :stdc) (:arguments ~{~s~}) (:return-type ~s))~%"
		lisp-name c-name args the-retval))

      #||
      (when +export-functions+
	(print-restrict when-restrict stream)
	(export-name lisp-name stream)
	)
      ||#
      
      (terpri stream)
      )))

;;; ====

  
(defmethod get-type-translation (type backend)
  type)

(defmethod get-type-translation (type (backend (eql :cmucl)))
  (case type
    (uchar8 'c-call:unsigned-char)
    (char8 'c-call:char)
    (c-string8 'c-call:c-string)
    (char-arr '(* char))
    (int32 'c-call:int)
    (int 'c-call:int)
    (long 'c-call:long)
    (short 'c-call:short)
    (void 'c-call:void)
    (char 'c-call:char)
    (unsigned 'alien:unsigned)
    (ptr-type 'alien:unsigned)
    (otherwise type)
    ))

(defmethod get-type-translation (type (backend (eql :sbcl)))
  (case type
    (uchar8 'unsigned-char)
    (char8 'char)
    (c-string8 'c-string)
    (char-arr '(* char))
    (int32 'int)
    (int 'int)
    (long 'long)
    (short 'short)
    (void 'void)
;;    (void 'void)
    (char 'char)
    (unsigned 'unsigned)
    (ptr-type 'unsigned)
    (otherwise type)
    ))


(defmethod get-type-translation (type (backend (eql :openmcl)))
  (case type
    (uchar8 :unsigned-byte)
    (char8 :signed-byte)
    (c-string8 :address)
    (char-arr :address)
    (int32 :signed-fullword)
    (int :signed-fullword)
    (long 'long)
    (short :signed-halfword)
    (void :void)
    (char 'char)
    (unsigned :unsigned-fullword)
    (ptr-type :address)
    (otherwise type)
    ))


(defmethod get-type-translation (type (backend (eql :clisp)))
  
  (when (and (consp type) (eq (car type) '*))
    (return-from get-type-translation (list 'c-ptr
					    (get-type-translation (cadr type) backend))
		 ))
  
  (case type
    (uchar8 'uchar)
    (char8 'char)
    (c-string8 'c-string)
    (cptr 'c-string)
    (char-arr 'c-string) ;; fix later
    (int32 'int)
    (int 'int)
    (void nil)
    (char 'char)
    (unsigned 'uint)
    (ptr-type 'c-pointer)
    (otherwise type)
    ))

(defmethod get-type-translation (type (backend (eql :acl)))
  #||
  (when (and (consp type) (eq (car type) '*))
    (return-from get-type-translation (list 'c-ptr
					    (get-type-translation (cadr type) backend))
		 ))
  ||#  
  (case type
    (uchar8 :unsigned-char)
    (char8 :char)
    (c-string8 '(* :char))
;;    (char-arr (values '(* :char) '(simple-array (unsigned-byte 8) (*))))
;;    (char-arr (values '(* (* :char)) 'integer))
    (char-arr (values '(* :void)))
    (int32 :int)
    (int :int)
    (short :short)
    (long :long)
    (void :void)
    (char :char)
    (unsigned :unsigned-long)
    (ptr-type nil)
    (otherwise type)
    ))

(defmethod get-type-translation (type (backend (eql :lispworks)))

  (case type
    (uchar8 :char)
    (char8 :char)
;; gradually turn stuff back to this way of doing strings, now that strings aren't
;; relevant in the main loop?
;; (:reference-pass :ef-mb-string)    
    (char-arr '(:reference-pass :ef-mb-string))
    (c-string8 '(:reference-pass :ef-mb-string))
    (ptr-type :ptr)
    (cptr '(:reference-pass :ef-mb-string))
    (int :int)
    (int32 :int)
    (long :long)
    (short :short)
    (void :void)
    (char :char)
    (unsigned :unsigned)
    (otherwise type)))

;;; ====

 

(defmethod generate-foreign-types (foreign-list stream (backend (eql :cmucl)))
  (dolist (i foreign-list)
    (let ((o-name (foreign-type-old-name i))
	  (n-name (foreign-type-new-name i)))
      (format stream "(alien:def-alien-type ~a ~s)~%" n-name (get-type-translation o-name backend))
      
      (when +export-types+
;;	(print-restrict when-restrict stream)
	(export-name n-name stream))
      (terpri stream)
      
      )))

(defmethod generate-foreign-types (foreign-list stream (backend (eql :sbcl)))
  (dolist (i foreign-list)
    (let ((o-name (foreign-type-old-name i))
	  (n-name (foreign-type-new-name i)))
      (format stream "~&(sb-alien:define-alien-type ~a ~s)~%" n-name (get-type-translation o-name backend))
      
      (when +export-types+
;;	(print-restrict when-restrict stream)
	(export-name n-name stream))
      (terpri stream)
      
      )))


(defmethod generate-foreign-types (foreign-list stream (backend (eql :acl)))
  (dolist (i foreign-list)
    (let ((o-name (foreign-type-old-name i))
	  (n-name (foreign-type-new-name i))
	  )
      (format stream "~&(ff:def-foreign-type ~a ~s)~%" n-name (get-type-translation o-name backend))
      
;;      (when +export-types+
;;;;	(print-restrict when-restrict stream)
;;	(export-name n-name stream))
      (terpri stream)
      
      )))

(defmethod generate-foreign-types (foreign-list stream (backend (eql :openmcl)))
  (dolist (i foreign-list)
    (let ((o-name (foreign-type-old-name i))
	  (n-name (foreign-type-new-name i))
	  )
      (format stream "~&(ccl::def-foreign-type ~a ~s)~%" n-name (get-type-translation o-name backend))
      
      (terpri stream)
      )))

(defmethod generate-foreign-types (foreign-list stream (backend (eql :lispworks)))
  (dolist (i foreign-list)
    (let ((o-name (foreign-type-old-name i))
	  (n-name (foreign-type-new-name i))
	  )
      (format stream "~&(fli:define-foreign-type ~a () '~s)~%" n-name
	      (get-type-translation o-name backend))

      (terpri stream)
      
      )))


(defmethod generate-foreign-types (foreign-list stream (backend (eql :clisp)))
  (dolist (i foreign-list)
    (let ((o-name (foreign-type-old-name i))
	  (n-name (foreign-type-new-name i))
	  )
      (format stream "~&(ffi:def-c-type ~a ~s)~%" n-name (get-type-translation o-name backend))
      
;;      (when +export-types+
;;;;	(print-restrict when-restrict stream)
;;	(export-name n-name stream))
      (terpri stream)
      
      )))

;;; ====



(defmethod generate-header (stream backend)
  (declare (ignore backend))
;;  (warn "generating header for generic..")
  (format stream ";;; Please do not edit this _GENERATED_ file.~2%")
  (pprint `(in-package ,+result-package+)
	  stream)
  (terpri stream)
  )

(defmethod generate-footer (stream backend)
;;  (warn "generating footer for generic..")
  (declare (ignore backend))
  (format stream "~2&;;; End of generated file.~%")
  )

(defmethod generate-export-clause (funs stream backend)
  (declare (ignore backend))
  (let ((names (mapcar #'foreign-fun-lisp-name funs)))
    (pprint `(eval-when (:execute :load-toplevel :compile-toplevel)
	      (export ',names))
	    stream)))
  
(defun generate-for-type (type fname)
  "Generates for a given type to the given file name."
  (let ((*print-case* :downcase))
    (with-open-file (s (pathname fname)
		       :direction :output
		       :if-exists :supersede)
      (generate-header s type)
      (generate-foreign-types (reverse *foreign-types*) s type)
      (format s "~2&")
      (let ((rev-fun (reverse *foreign-functions*)))
	(generate-foreign-functions rev-fun  s type)

	(generate-export-clause rev-fun s type))
     
      
      (generate-footer s type)
      )))


(defun clean-foreign-tables ()
  "Cleans up tables."
  (setq *foreign-types* nil
	*foreign-functions* nil
	*names-to-export* nil))
