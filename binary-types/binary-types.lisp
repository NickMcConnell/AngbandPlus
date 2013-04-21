;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 200120001999,
;;;;    Department of Computer Science, University of Tromsø, Norway
;;;; 
;;;; Filename:      binary-types.lisp
;;;; Description:   Reading and writing of binary data in streams.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Fri Nov 19 18:53:57 1999
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: binary-types.lisp,v 1.4 2001/04/22 16:34:35 stig Exp $
;;;;                
;;;;------------------------------------------------------------------

(defpackage #:binary-types
  (:nicknames #:bt)
  (:use #:common-lisp)
  (:export *endian*			; [dynamic-var] must be bound when reading integers
	   ;; built-in types
	   char8			; [type-name] 8-bit character
	   u8				; [type-name] 8-bit unsigned integer
	   u16				; [type-name] 16-bit unsigned integer
	   u32				; [type-name] 32-bit unsigned integer
	   s8				; [type-name] 8-bit signed integer
	   s16				; [type-name] 16-bit signed integer
	   s32				; [type-name] 32-bit signed integer
					; (you may define additional integer types
					; of any size yourself.)
	   ;; type declarators
	   define-unsigned		; [macro] declare an unsigned-int type of any byte-size
	   define-signed		; [macro] declare a signed-int type of any byte-size
	   define-binary-struct		; [macro] declare a compound [record] defstruct type
	   define-binary-class		; [macro] declare a compound [record] defclass type
	   define-bitfield		; [macro] declare a bitfield (symbolic integer) type
	   define-enum			; [macro] declare an enumerated type
	   define-fixed-size-nt-string	; [macro] declare a null-terminated, size-bound type
	   find-binary-type		; [func] accessor to binary-types namespace
	   ;; readers and writers
	   read-binary			; [func] reads a binary-type from a stream
	   write-binary			; [func] writes an object as a binary-type to a stream
	   write-binary-compound
	   read-fixed-size-string
	   read-fixed-size-nt-string
	   ;; compound handling
	   compound-slot-names		; [func] list names of binary slots.
	   slot-offset			; [func] determine offset of slot.
	   ;; misc
	   sizeof			; [func] The size in octets of a binary type
	   enum-value			; [func] Calculate numeric version of enum value
	   with-binary-file		; [macro] variant of with-open-file
	   with-binary-output-to-list	; [macro]
	   *binary-write-byte*		; [dynamic-var]
	   *binary-read-byte*		; [dynamic-var]
	   ))

(in-package #:binary-types)

(defvar *binary-write-byte* #'cl:write-byte
  "The low-level WRITE-BYTE function used by binary-types.")
(defvar *binary-read-byte*  #'cl:read-byte
  "The low-level READ-BYTE function used by binary-types.")

;;; ----------------------------------------------------------------
;;;                         Utilities
;;; ----------------------------------------------------------------

(defun make-pairs (list)
  "(make-pairs '(1 2 3 4)) => ((1 . 2) (3 . 4))"
  (when list
    (cons (cons (first list)
                (second list))
          (make-pairs (rest (rest list))))))

;;; ----------------------------------------------------------------
;;; 
;;; ----------------------------------------------------------------

(eval-when (compile eval load)
  (deftype endianess ()
    "These are the legal declarations of endianess. The value NIL
means that the endianess is determined by the dynamic value of *endian*."
    '(member nil :big-endian :little-endian)))

(defvar *endian* nil
  "*endian* must be (dynamically) bound to either :big-endian or
:little-endian while reading endian-sensitive types.")

;;; ----------------------------------------------------------------
;;;                  Binary Types Namespace
;;; ----------------------------------------------------------------

(defvar *binary-type-namespace* (make-hash-table :test #'eq)
  "Maps binary type's names (which are symbols) to their binary-type
class.")

(defun find-binary-type (name &optional (errorp t))
  (or (gethash name *binary-type-namespace*)
      (if errorp
	  (error "Unable to find binary type ~A." name)
	nil)))

(defun (setf find-binary-type) (value name)
  (check-type value binary-type)
  (let ((old-value (find-binary-type name nil)))
    (when (and old-value (not (eq (class-of value) (class-of old-value))))
      (warn "Redefining binary-type ~A from ~A to ~A."
	    name (type-of old-value) (type-of value))))
  (setf (gethash name *binary-type-namespace*) value))

(defun find-binary-type-name (type)
  (maphash #'(lambda (key val)
	       (when (eq type val)
		 (return-from find-binary-type-name key)))
	   *binary-type-namespace*))

;;; ----------------------------------------------------------------
;;;                  Base Binary Type (Abstract)
;;; ----------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass binary-type ()
  ((name :initarg name
	 :initform 'anonymous
	 :reader binary-type-name))
  (:documentation "BINARY-TYPE is the base class for binary types."))
)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defgeneric sizeof (type)
  (:documentation "Return the size in octets of the single argument TYPE,
or nil if TYPE is not constant-sized."))
)

(defmethod sizeof (obj)
  (sizeof (find-binary-type (type-of obj))))
  
(defmethod sizeof ((type symbol))
  (sizeof (find-binary-type type)))

  

(eval-when (:compile-toplevel :load-toplevel :execute)

(defgeneric sizeof-min (type)
  (:documentation "Return minimum size."))
)

(defmethod sizeof-min ((type symbol))
  (sizeof-min (find-binary-type type)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defgeneric sizeof-max (type)
  (:documentation "Return maximum size."))
)

(defmethod sizeof-max ((type symbol))
  (sizeof-max (find-binary-type type)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defgeneric read-binary (type stream &key &allow-other-keys)
  (:documentation "Read an object of binary TYPE from STREAM."))
)

(defmethod read-binary ((type symbol) stream &rest key-args)
  (apply #'read-binary (find-binary-type type) stream key-args))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defgeneric write-binary (type stream object &key &allow-other-keys)
  (:documentation "Write an OBJECT of TYPE to STREAM."))
)

(defmethod write-binary ((type symbol) stream object &rest key-args)
  (apply #'write-binary (find-binary-type type) stream object key-args))

;;; ----------------------------------------------------------------
;;;                  Constant-Sized Types (Abstract)
;;; ----------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass binary-type-constant-size (binary-type)
  ((sizeof :type unsigned-byte
	   :accessor sizeof
	   :initarg sizeof)))
)

(defmethod sizeof-min ((type binary-type-constant-size))
  (sizeof type))

(defmethod sizeof-max ((type binary-type-constant-size))
  (sizeof type))

;;; ----------------------------------------------------------------
;;;                  Variable-Sized Types (Abstract)
;;; ----------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass binary-type-variable-size (binary-type)
  ((sizeof-min :type (or nil unsigned-byte)
	       :accessor sizeof-min
	       :initarg sizeof-min)
   (sizeof-max :type (or nil unsigned-byte)
	       :accessor sizeof-max
	       :initarg sizeof-max)))
)

(defmethod sizeof ((type binary-type-variable-size))
  (if (eq (slot-value type 'sizeof-min)
	  (slot-value type 'sizeof-max))
      (slot-value type 'sizeof-min)
    nil))

;;; ----------------------------------------------------------------
;;;                      Integer Type (Abstract)
;;; ----------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass binary-integer (binary-type-constant-size)
  ((endian :type endianess
	   :reader binary-integer-endian
	   :initarg endian
	   :initform nil)))
)

;;; WRITE-BINARY is identical for SIGNED and UNSIGNED, but READ-BINARY
;;; is not.

(defmethod write-binary ((type binary-integer) stream object &key &allow-other-keys)
  (if (= 1 (sizeof type))
      (progn (funcall *binary-write-byte* object stream) 1)
    (ecase (or (binary-integer-endian type)
	       *endian*)
      ((:big-endian big-endian)
       (do ((i (* 8 (1- (sizeof type))) (- i 8)))
	   ((minusp i) (sizeof type))
	 (funcall *binary-write-byte* (ldb (byte 8 i) object) stream)))
      ((:little-endian little-endian)
       (dotimes (i (sizeof type))
	 (funcall *binary-write-byte* (ldb (byte 8 (* 8 i)) object) stream))
       (sizeof type)))))


;;; ----------------------------------------------------------------
;;;                      Unsigned Integer Types
;;; ----------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass binary-unsigned (binary-integer) ())
)

(defmacro define-unsigned (name size &optional endian)
  (check-type size (integer 1 *))
  (check-type endian endianess)
  `(progn
     (deftype ,name () '(unsigned-byte ,(* 8 size)))
     (setf (find-binary-type ',name)
       (make-instance 'binary-unsigned
	 'name ',name
	 'sizeof ,size
	 'endian ,endian))
     ',name))

(defmethod print-object ((type binary-unsigned) stream)
  (if (not *print-readably*)
      (format stream "#<BINARY-UNSIGNED ~D-BIT~@[ ~A~] ~A>"
	      (* 8 (slot-value type 'sizeof))
	      (slot-value type 'endian)
	      (binary-type-name type))
    (call-next-method type stream)))


(define-unsigned u8 1)
(define-unsigned u16 2)
(define-unsigned u32 4)


(defmethod read-binary ((type binary-unsigned) stream &key &allow-other-keys)
  (if (= 1 (sizeof type))
      (values (funcall *binary-read-byte* stream)
	      1)
    (let ((unsigned-value 0))
      (ecase (or (binary-integer-endian type)
		 *endian*)
	((:big-endian big-endian)
	 (dotimes (i (sizeof type))
	   (setf unsigned-value (+ (* unsigned-value #x100)
				   (funcall *binary-read-byte* stream)
				   ))))
	((:little-endian little-endian)
	 (dotimes (i (sizeof type))
	   (setf unsigned-value (+ unsigned-value
				   (ash (funcall *binary-read-byte* stream)
					(* 8 i)))
		 ))))
      (values unsigned-value
	      (sizeof type)))))
    
;;; ----------------------------------------------------------------
;;;              Twos Complement Signed Integer Types
;;; ----------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass binary-signed (binary-integer) ())
)

(defmacro define-signed (name size &optional (endian nil))
  (check-type size (integer 1 *))
  (check-type endian endianess)
  `(progn
     (deftype ,name () '(signed-byte ,(* 8 size)))
     (setf (find-binary-type ',name)
       (make-instance 'binary-signed
	 'name ',name
	 'sizeof ,size
	 'endian ,endian))
     ',name))

(defmethod print-object ((type binary-signed) stream)
  (if (not *print-readably*)
      (format stream "#<BINARY-SIGNED ~D-BIT~@[ ~A~] ~A>"
	      (* 8 (slot-value type 'sizeof))
	      (slot-value type 'endian)
	      (binary-type-name type))
    (call-next-method type stream)))


(define-signed s8 1)
(define-signed s16 2)
(define-signed s32 4)


(defmethod read-binary ((type binary-signed) stream &key &allow-other-keys)
  (let ((unsigned-value 0))
    (if (= 1 (sizeof type))
	(setf unsigned-value (funcall *binary-read-byte* stream))
      (ecase (or (binary-integer-endian type)
		 *endian*)
	((:big-endian big-endian)
	 (dotimes (i (sizeof type))
	   (setf unsigned-value (+ (* unsigned-value #x100)
				   (funcall *binary-read-byte* stream)
				   ))))
	((:little-endian little-endian)
	 (dotimes (i (sizeof type))
	   (setf unsigned-value (+ unsigned-value
				   (ash (funcall *binary-read-byte* stream)
					(* 8 i))))))))
    (values (if (>= unsigned-value (ash 1 (1- (* 8 (sizeof type)))))
		(- unsigned-value (ash 1 (* 8 (sizeof type))))
	      unsigned-value)
	    (sizeof type))))

;;; ----------------------------------------------------------------
;;;                       Character Types
;;; ----------------------------------------------------------------

;;; There are probably lots of things one _could_ do with character
;;; sets etc..

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass binary-char8 (binary-type-constant-size) ())
)

(setf (find-binary-type 'char8)
  (make-instance 'binary-char8
    'name 'char8
    'sizeof 1))

(eval-when (:compile-toplevel :load-toplevel :execute)
(deftype char8 () 'character)
)

(defmethod read-binary ((type binary-char8) stream &key &allow-other-keys)
  (values (code-char (read-binary 'u8 stream))
	  1))

(defmethod write-binary ((type binary-char8) stream object &key &allow-other-keys)
  (write-binary 'u8 stream (char-code object)))

;;; ----------------------------------------------------------------
;;;             Padding Type (Implicitly named by integers)
;;; ----------------------------------------------------------------

;;; The padding type of size 3 octets is named by the integer 3, and
;;; so on. You may find it useful to have zero-sized padding fields in
;;; binclasses to emulate "labels".

(defmethod sizeof ((type integer)) type)
(defmethod sizeof-min ((type integer)) type)
(defmethod sizeof-max ((type integer)) type)

(defmethod read-binary ((type integer) stream &key &allow-other-keys)
  (dotimes (i type)
    (read-binary 'u8 stream))
  (values nil type))

(defvar *padding-byte* #x00
  "The value written to padding octets.")

(defmethod write-binary ((type integer) stream object &key &allow-other-keys)
  (declare (ignore object))
  (dotimes (i type)
    (write-binary 'u8 stream *padding-byte*))
  type)

;;; ----------------------------------------------------------------
;;;                  Null-Terminated String Types
;;; ----------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass fixed-size-nt-string (binary-type-constant-size) ())
)

(defmacro define-fixed-size-nt-string (type-name size)
  (check-type size (integer 1 *))
  `(progn
     (deftype ,type-name () 'string)
     (setf (find-binary-type ',type-name)
       (make-instance 'fixed-size-nt-string
	 'name ',type-name
	 'sizeof ,size))
     ',type-name))

(defun read-fixed-size-nt-string (size stream)
  (let ((string (make-string size)))
    (dotimes (i size)
      (setf (aref string i) (code-char (funcall *binary-read-byte* stream))))
    (values (subseq string 0 (position #\null string))
	    size)))

(defmethod read-binary ((type fixed-size-nt-string) stream &key &allow-other-keys)
  (read-fixed-size-nt-string (sizeof type) stream))

(defmethod write-binary ((type fixed-size-nt-string) stream obj  &key &allow-other-keys)
  (dotimes (i (sizeof type))
    (if (< i (length obj))
	(funcall *binary-write-byte* (char-code (aref obj i)) stream)
      (funcall *binary-write-byte* 0 stream)))
  (sizeof type))

;;; ----------------------------------------------------------------
;;;                         String Types
;;; ----------------------------------------------------------------

(defun read-fixed-size-string (size stream)
  (let ((string (make-string size)))
    (dotimes (i size)
      (setf (aref string i) (code-char (funcall *binary-read-byte* stream))))
    (values string
	    size)))

;;; ----------------------------------------------------------------
;;;                    Compound Types ("structs")
;;; ----------------------------------------------------------------

;;;(defstruct compound-slot
;;;  name
;;;  type
;;;  on-write)

(defun make-compound-slot (&key name type on-write)
  (list name type on-write))

(defun compound-slot-name (s)
  (first s))
(defun compound-slot-type (s)
  (second s))
(defun compound-slot-on-write (s)
  (third s))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass compound (binary-type-variable-size)
  ((slots  :initarg slots
	   :accessor compound-slots)
   (offset :initarg offset
	   :reader compound-class-slot-offset)))

(defclass compound-class (compound)
  ((instance-class :type standard-class
		   :initarg instance-class)))
)

(defmethod compound-make-instance ((type compound-class))
  (make-instance (slot-value type 'instance-class)))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass compound-struct (compound)
  ((constructor :initarg constructor)))
)

(defmethod compound-make-instance ((type compound-struct))
  (funcall (slot-value type 'constructor)))

(defun slot-offset (type slot-name)
  "Return the offset (in number of octets) of SLOT-NAME in TYPE."
  (unless (typep type 'compound)
    (setf type (find-binary-type type)))
  (unless (typep type 'compound)
    (error "~S is not of COMPOUND type."))
  (unless (find-if #'(lambda (slot)
		       (eq slot-name (compound-slot-name slot)))
		   (compound-slots type))
    (error "Slot ~S doesn't exist in type ~S."
	   slot-name type))
  (+ (compound-class-slot-offset type)
     (loop for slot in (compound-slots type)
	 until (eq slot-name (compound-slot-name slot))
	 summing (sizeof (compound-slot-type slot)))))

(defun compound-slot-names (type &optional (padding-slots-p nil))
  "Returns a list of the slot-names of TYPE, in sequence."
  (unless (typep type 'compound)
    (setf type (find-binary-type type)))
  (if padding-slots-p
      (mapcar #'compound-slot-name (compound-slots type))
    (mapcan #'(lambda (slot)
		(if (integerp (compound-slot-type slot))
		    nil
		  (list (compound-slot-name slot))))
	    (compound-slots type))))

(defmacro define-compound (type-name slot-specs)
  "Support for old syntax. Deprecated."
  `(define-binary-class ,type-name ()
     ,(mapcar #'(lambda (slot-spec)
		  (list (first slot-spec)
			:bt
			(second slot-spec)))
	      slot-specs)))

(defun quoted-name-p (form)
  (and (listp form)
       (= 2 (length form))
       (eq 'cl:quote (first form))
       (symbolp (second form))
       (second form)))

(defun parse-bt-spec (expr)
  "Takes a binary-type specifier (a symbol, integer, or define-xx form),
  and returns three values: the binary-type's name, the equivalent lisp type,
  and any nested declaration that must be expanded separately."
  (cond
   ((symbolp expr) (values expr expr))	; a name
   ((integerp expr) (values expr nil))	; a padding type
   ((quoted-name-p expr)
    (values (second expr) (second expr))) ; a quoted name
   ((and (listp expr)			; a nested declaration
	 (member (first expr)
		 '(define-unsigned define-signed define-enum define-bitfield
		   define-binary-class define-binary-struct
		   define-fixed-size-nt-string)))
    (values (second expr) (second expr) expr))
   (t (error "Unknown nested binary-type specifier: ~S" expr))))

(defmacro define-binary-class (type-name supers slots &rest class-options)
  (let (embedded-declarations)
    (flet ((parse-slot-specifier (slot-specifier)
	     "For a class slot-specifier, return the slot-specifier to forward
             (sans binary-type options), the binary-type of the slot (or nil),
             and the slot's name."
	     (if (symbolp slot-specifier)
		 (values slot-specifier nil slot-specifier)
	       (loop for slot-option on (rest slot-specifier) by #'cddr
		   with bintype = nil
		   and typetype = nil
		   and on-write = nil
		   if (eq :bt-on-write (first slot-option))
		   do (setf on-write (second slot-option))
		   else if (member (first slot-option) '(:bt :btt))
		   do (multiple-value-bind (bt tt nested-form)
			  (parse-bt-spec (second slot-option))
			(setf bintype bt)
			(when nested-form
			  (push nested-form embedded-declarations))
			(when (and (symbolp tt)
				   (eq :btt (first slot-option)))
			  (setf typetype tt)))
		   else nconc (list (first slot-option)
				    (second slot-option)) into options
		   finally (return (values (list* (first slot-specifier)
						  (if typetype
						      (list* :type typetype options)
						    options))
					   bintype
					   (first slot-specifier)
					   on-write))))))
      (let* ((binslots (mapcan #'(lambda (slot-specifier)
				   (multiple-value-bind (options bintype slot-name on-write)
				       (parse-slot-specifier slot-specifier)
				     (declare (ignore options))
				     (if bintype
					 (list (make-compound-slot
						:name slot-name
						:type bintype
						:on-write on-write))
				       nil)))
			       slots))
	     (slot-types (mapcar #'compound-slot-type binslots))
	     (forward-class-options (loop for co in class-options
					unless (member (car co)
						       '(:slot-align :class-slot-offset))
					collect co))
	     (class-slot-offset (or (second (assoc :class-slot-offset class-options)) 0))
	     (slot-align-slot (second (assoc :slot-align class-options)))
	     (slot-align-offset (third (assoc :slot-align class-options))))
	`(progn
	   ,@embedded-declarations
	   (defclass ,type-name ,supers
	     ,(mapcar #'parse-slot-specifier slots)
	     ,@forward-class-options)
	   (let ((compound-size-min (let ((min (mapcar #'sizeof-min ',slot-types)))
				      (if (notany #'null min)
					  (reduce #'+ min)
					nil)))
		 (compound-size-max (let ((max (mapcar #'sizeof-max ',slot-types)))
				      (if (notany #'null max)
					  (reduce #'+ max)
					nil))))
	     (setf (find-binary-type ',type-name)
	       (make-instance 'compound-class
		 'name ',type-name
		 'sizeof-min compound-size-min
		 'sizeof-max compound-size-max
		 'slots ',binslots
		 'offset ,class-slot-offset
		 'instance-class (find-class ',type-name)))
	     ,@(when slot-align-slot
		 `((setf (slot-value (find-binary-type ',type-name) 'offset)
		     (- ,slot-align-offset
			(slot-offset ',type-name ',slot-align-slot)))))
	     ',type-name))))))
  

(defmacro define-binary-struct (name-and-options dummy-options &rest doc-slot-descriptions)
  (declare (ignore dummy-options))	; clisp seems to require this..
  (let (embedded-declarations)
    (flet ((parse-slot-description (slot-description)
	     (cond
	      ((symbolp slot-description)
	       (values slot-description nil slot-description))
	      ((>= 2 (list-length slot-description))
	       (values slot-description nil (first slot-description)))
	      (t (loop for descr on (cddr slot-description) by #'cddr
		     with bintype = nil
		     and typetype = nil
		     if (member (first descr) '(:bt :btt))
		     do (multiple-value-bind (bt lisp-type nested-form)
			    (parse-bt-spec (second descr))
			  (declare (ignore lisp-type))
			  (setf bintype bt)
			  (when nested-form
			    (push nested-form embedded-declarations))
			  (when (and (symbolp bt)
				     (eq :btt (first descr)))
			    (setf typetype bintype)))
		     else nconc (list (first descr) (second descr)) into descriptions
		     finally (return (values (list* (first slot-description)
						    (second slot-description)
						    (if typetype
							(list* :type typetype descriptions)
						      descriptions))
					     bintype
					     (first slot-description))))))))
      (multiple-value-bind (doc slot-descriptions)
	  (if (stringp (first doc-slot-descriptions))
	      (values (list (first doc-slot-descriptions))
		      (rest doc-slot-descriptions))
	    (values nil doc-slot-descriptions))
	(let* ((type-name (if (consp name-and-options)
			      (first name-and-options)
			    name-and-options))
	       (binslots (mapcan (lambda (slot-description)
				   (multiple-value-bind (options bintype slot-name)
				       (parse-slot-description slot-description)
				     (declare (ignore options))
				     (if bintype
					 (list (make-compound-slot :name slot-name
								   :type bintype))
				       nil)))
				 slot-descriptions))
	       (slot-types (mapcar #'compound-slot-type binslots)))
	  `(progn
	     ,@embedded-declarations
	     (let* ((compound-size-min (let ((min (mapcar #'sizeof-min ',slot-types)))
					 (if (notany #'null min)
					     (reduce #'+ min)
					   nil)))
		    (compound-size-max (let ((max (mapcar #'sizeof-max ',slot-types)))
					 (if (notany #'null max)
					     (reduce #'+ max)
					   nil))))
	       (defstruct ,name-and-options
		 ,@doc
		 ,@(mapcar #'parse-slot-description slot-descriptions))
	       (setf (find-binary-type ',type-name)
		 (make-instance 'compound-struct
		   'name ',type-name
		   'sizeof-min compound-size-min
		   'sizeof-max compound-size-max
		   'slots ',binslots
		   'offset 0
		   'constructor (find-symbol (format nil "~A-~A" '#:make ',type-name))))
	       ',type-name)))))))


(defmethod read-binary ((type compound) stream &key start stop &allow-other-keys)
  (let ((start-slot 0)
	(stop-slot nil))
    (when start
      (setf start-slot (position-if #'(lambda (sp)
					(eq start (compound-slot-name sp)))
				    (compound-slots type)))
      (unless start-slot
	(error "start-slot ~S not found in type ~A"
	       start type)))
    (when stop
      (setf stop-slot (position-if #'(lambda (sp)
				       (eq stop (compound-slot-name sp)))
				   (compound-slots type)))
      (unless stop-slot
	(error "stop-slot ~S not found in type ~A"
	       stop  type)))
    (let ((read-bytes 0)
	  (slot-list (subseq (compound-slots type) start-slot stop-slot))
	  (obj (compound-make-instance type)))
      (dolist (slot slot-list)
	(multiple-value-bind (slot-obj slot-bytes)
	    (read-binary (compound-slot-type slot) stream)
	  (setf (slot-value obj (compound-slot-name slot)) slot-obj)
	  (incf read-bytes slot-bytes)))
      (values obj read-bytes))))


(defmethod write-binary-compound (object stream)
  (write-binary (find-binary-type (type-of object)) stream object))

(defmethod write-binary ((type compound) stream object
			 &key start stop &allow-other-keys)
  (let ((start-slot 0)
	(stop-slot nil))
    (when start
      (setf start-slot (position-if #'(lambda (sp)
					(eq start (compound-slot-name sp)))
				    (compound-slots type)))
      (unless start-slot
	(error "start-slot ~S not found in type ~A"
	       start type)))
    (when stop
      (setf stop-slot (position-if #'(lambda (sp)
				       (eq stop (compound-slot-name sp)))
				   (compound-slots type)))
      (unless stop-slot
	(error "stop-slot ~S not found in type ~A"
	       stop type)))
    (let ((written-bytes 0)
	  (slot-list (subseq (compound-slots type) start-slot stop-slot)))
      (dolist (slot slot-list)
	(let* ((slot-name (compound-slot-name slot))
	       (slot-type (compound-slot-type slot))
	       (value (cond
		       ((integerp slot-type) nil) ; padding
		       ((compound-slot-on-write slot)
			(funcall (compound-slot-on-write slot)
				 (slot-value object slot-name)))
		       (t (slot-value object slot-name)))))
	  (incf written-bytes
		(write-binary slot-type stream value))))
      written-bytes)))

(defun compound-merge (obj1 obj2)
  "Returns a compound where every non-bound slot in obj1 is replaced
with that slot's value from obj2."
  (let ((class (class-of obj1)))
    (unless (eq class (class-of obj2))
      (error "cannot merge incompatible compounds ~S and ~S" obj1 obj2))
    (let ((new-obj (make-instance class)))
      (dolist (slot (compound-slots (find-binary-type (type-of obj1))))
	(let ((slot-name (compound-slot-name slot)))
	  (cond
	   ((slot-boundp obj1 slot-name)
	    (setf (slot-value new-obj slot-name)
	      (slot-value obj1 slot-name)))
	   ((slot-boundp obj2 slot-name)
	    (setf (slot-value new-obj slot-name)
	      (slot-value obj2 slot-name))))))
      new-obj)))

(defun compound-alist (obj)
  "Returns an assoc-list representation of (the slots of) a binary
compound object."
  (mapcan #'(lambda (slot)
	      (unless (integerp (compound-slot-type slot))
		(list (cons (compound-slot-name slot)
			    (if (slot-boundp obj (compound-slot-name slot))
				(slot-value obj (compound-slot-name slot))
			      'unbound-slot)))))
	  (compound-slots (find-binary-type (type-of obj)))))

;;; ----------------------------------------------------------------
;;; Bitfield Types
;;; ----------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass bitfield (binary-type-constant-size)
  ((storage-type :type t
		 :accessor storage-type
		 :initarg storage-type)
   (hash  :type hash-table
	  :initform (make-hash-table :test #'eq)
	  :accessor bitfield-hash)))

(defstruct bitfield-entry
  value
  bytespec))

(defmacro define-bitfield (type-name (storage-type) spec)
  (let ((slot-list			; (slot-name value byte-size byte-pos)
	 (mapcan #'(lambda (set)
		     (ecase (caar set)
		       ((:bits)
			(mapcar #'(lambda (slot)
				    (list (car slot)
					  1
					  1
					  (cdr slot)))
				(make-pairs (cdr set))))
		       ((:enum)
			(destructuring-bind (&key byte)
			    (rest (car set))
			  (mapcar #'(lambda (slot)
				      (list (car slot)
					    (cdr slot)
					    (first byte)
					    (second byte)))
				  (make-pairs (cdr set)))))
		       ((:numeric)
			(let ((s (car set)))
			  (list (list (second s)
				      nil
				      (third s)
				      (fourth s)))))))
		 spec)))
    `(let ((type-obj (make-instance 'bitfield 
		       'name ',type-name
		       'storage-type (find-binary-type ',storage-type))))
       (deftype ,type-name () '(or list symbol))
       (setf (sizeof type-obj) (sizeof (storage-type type-obj)))
       (dolist (slot ',slot-list)
	 (setf (gethash (first slot) (bitfield-hash type-obj))
	   (make-bitfield-entry :value (second slot)
				:bytespec (if (and (third slot)
						   (fourth slot))
					      (byte (third slot)
						    (fourth slot))
					    nil))))
       (setf (find-binary-type ',type-name) type-obj)
       ',type-name)))

(defmacro define-enum (type-name (storage-name &optional byte-spec) &rest spec)
  "A simple wrapper around DEFINE-BITFIELD for simple enum types."
  `(define-bitfield ,type-name (,storage-name)
     (((:enum :byte ,byte-spec)
       ,@spec))))

(defun bitfield-compute-symbolic-value (type numeric-value)
  "Return the symbolic value of a numeric bitfield"
  (check-type numeric-value integer)
  (let (result)
    (maphash #'(lambda (slot-name entry)
		 (let ((e-value (bitfield-entry-value entry))
		       (e-bytespec (bitfield-entry-bytespec entry)))
		   (cond
		    ((and e-value e-bytespec)
		     (when (= e-value
			      (ldb e-bytespec numeric-value))
		       (push slot-name
			     result)))
		    (e-value
		     ;; no mask => this must be the sole entry present
		     (when (= numeric-value e-value)
		       (setf result slot-name)))
		    (e-bytespec
		     ;; no value => this is a numeric sub-field
		     (push (cons slot-name
				 (ldb e-bytespec numeric-value))
			   result))
		    (t (error "bitfield-value type ~A has NIL value and bytespec" type)))))
	     (bitfield-hash type))
;;;;; Consistency check by symmetry.
;;;    (unless (= numeric-value
;;;	       (bitfield-compute-numeric-value type result))
;;;      (error "bitfield inconsitency with ~A: ~X => ~A => ~X."
;;;	     (type-of type)
;;;	     numeric-value
;;;	     result
;;;	     (bitfield-compute-numeric-value type result)))
    result))

(defun enum-value (type symbolic-value)
  "For an enum type (actually, for any bitfield-based type), ~
   look up the numeric value of a symbol."
  (unless (typep type 'bitfield)
    (setf type (find-binary-type type)))
  (bitfield-compute-numeric-value type symbolic-value))

(defun bitfield-compute-numeric-value (type symbolic-value)
  "Returns the numeric representation of a bitfields symbolic value."
  (etypecase symbolic-value
    (list
     (let ((result 0))
       (dolist (slot symbolic-value)
	 (etypecase slot
	   (symbol			; enum sub-field
	    (let ((entry (gethash slot (bitfield-hash type))))
	      (assert entry (entry) "Unknown bitfield slot ~S of ~S."
		      slot (find-binary-type-name type))
	      (setf (ldb (bitfield-entry-bytespec entry) result)
		(bitfield-entry-value entry))))
	   (cons			; numeric sub-field
	    (let ((entry (gethash (car slot) (bitfield-hash type))))
	      (assert entry (entry) "Unknown bitfield slot ~S of ~S."
		      (car slot) (find-binary-type-name type))
	      (setf (ldb (bitfield-entry-bytespec entry) result)
		(cdr slot))))))
       result))
    (symbol				; enum
     (let ((entry (gethash symbolic-value
			   (bitfield-hash type))))
       (assert entry (entry) "Unknown bitfield slot ~A:~S of ~S."
	       (package-name (symbol-package symbolic-value))
	       symbolic-value
	       (find-binary-type-name type))
       (if (bitfield-entry-bytespec entry)
	   (dpb (bitfield-entry-value entry)
		(bitfield-entry-bytespec entry)
		0)
	 (bitfield-entry-value entry))))))
  
(defmethod read-binary ((type bitfield) stream &key &allow-other-keys)
  (multiple-value-bind (storage-obj num-octets-read)
      (read-binary (storage-type type) stream)
    (values (bitfield-compute-symbolic-value type storage-obj)
	    num-octets-read)))
  
(defmethod write-binary ((type bitfield) stream symbolic-value &rest key-args)
  (apply #'write-binary
	 (storage-type type)
	 stream
	 (bitfield-compute-numeric-value type symbolic-value)
	 key-args))

;;;;

(defmacro with-binary-file ((stream-var path &rest key-args) &body body)
  "This is a thin wrapper around WITH-OPEN-FILE, that tries to set the
stream's element-type to that required by READ-BINARY and WRITE-BINARY.
A run-time assertion on the stream's actual element type is performed,
unless you disable this feature by setting the keyword option :check-stream
to nil."
  (let ((check-stream (getf key-args :check-stream t))
	(fwd-key-args (copy-list key-args)))
    ;; This is manual parsing of keyword arguments. We force :element-type
    ;; to (unsigned-byte 8), and remove :check-stream from the arguments
    ;; passed on to WITH-OPEN-FILE.
    (remf fwd-key-args :check-stream)
    ;; #-(and allegro-version>= (version>= 6 0))
    (setf (getf fwd-key-args :element-type) ''(unsigned-byte 8))
    `(with-open-file (,stream-var ,path ,@fwd-key-args)
       ,@(when check-stream
	   `((let ((stream-type (stream-element-type ,stream-var)))
	       (assert (and (subtypep '(unsigned-byte 8) stream-type)
			    (subtypep stream-type '(unsigned-byte 8)))
		   ()
		 "Failed to open ~S in 8-bit binary mode, stream element-type was ~S"
		 ,path stream-type))))
       ,@body)))

(defmacro with-binary-output-to-list ((stream-var) &body body)
  "Inside BODY, calls to WRITE-BINARY with stream STREAM-VAR will
collect the individual bytes in a list (of integers).
This list is returned by the form. (There is no way to get at
the return-value of BODY.)"
  (let ((save-bwt-var (make-symbol "save-bwt"))
	(closure-byte-var (make-symbol "closure-byte"))
	(closure-stream-var (make-symbol "closure-stream")))
    `(let* ((,save-bwt-var *binary-write-byte*)
	    (,stream-var (cons nil nil)) ; (head . tail)
	    (*binary-write-byte*
	     #'(lambda (,closure-byte-var ,closure-stream-var)
		 (if (eq ,stream-var ,closure-stream-var)
		     (if (endp (cdr ,stream-var))
			 (setf (cdr ,stream-var)
			   (setf (car ,stream-var) (list ,closure-byte-var)))
		       (setf (cdr ,stream-var)
			 (setf (cddr ,stream-var) (list ,closure-byte-var))))
		   (funcall ,save-bwt-var ; it's not our stream, so pass it
			    ,closure-byte-var ; along to the next function.
			    ,closure-stream-var)))))
       ,@body
       (car ,stream-var))))
