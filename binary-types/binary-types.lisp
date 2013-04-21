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
;;;;------------------------------------------------------------------

(defpackage binary-types
  (:nicknames bt)
  (:use common-lisp)
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
	   ;; type defining macros
	   define-unsigned		; [macro] declare an unsigned-int type
	   define-signed		; [macro] declare a signed-int type
	   define-binary-struct		; [macro] declare a binary defstruct type
	   define-binary-class		; [macro] declare a binary defclass type
	   define-bitfield		; [macro] declare a bitfield (symbolic integer) type
	   define-enum			; [macro] declare an enumerated type
	   define-binary-string		; [macro] declare a string type
	   define-null-terminated-string; [macro] declare a null-terminated string
	   ;; readers and writers
	   read-binary			; [func] reads a binary-type from a stream
	   write-binary			; [func] writes an binary object to a stream
	   write-binary-record
	   read-binary-string
	   ;; record handling
	   binary-record-slot-names	; [func] list names of binary slots.
	   binary-slot-value		; [func] get "binary" version of slot's value
	   binary-slot-type		; [func] get binary slot's binary type
	   slot-offset			; [func] determine offset of slot.
	   ;; misc
	   find-binary-type		; [func] accessor to binary-types namespace
	   sizeof			; [func] The size in octets of a binary type
	   enum-value			; [func] Calculate numeric version of enum value
	   with-binary-file		; [macro] variant of with-open-file
	   with-binary-output-to-list	; [macro]
	   with-binary-output-to-vector	; [macro]
	   with-binary-input-from-list	; [macro]
	   with-binary-input-from-vector ; [macro]
	   *binary-write-byte*		; [dynamic-var]
	   *binary-read-byte*		; [dynamic-var]
	   *padding-byte*		; [dynamic-var] The value filled in when writing paddings
	   ))

(in-package binary-types)

(defvar *binary-write-byte* #'common-lisp:write-byte
  "The low-level WRITE-BYTE function used by binary-types.")
(defvar *binary-read-byte*  #'common-lisp:read-byte
  "The low-level READ-BYTE function used by binary-types.")

;;; ----------------------------------------------------------------
;;;                         Utilities
;;; ----------------------------------------------------------------

(defun make-pairs (list)
  "(make-pairs '(1 2 3 4)) => ((1 . 2) (3 . 4))"
  (loop for x on list by #'cddr collect (cons (first x) (second x))))

;;; ----------------------------------------------------------------
;;; 
;;; ----------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
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
  "Maps binary type's names (which are symbols) to their binary-type class object.")

(defun find-binary-type (name &optional (errorp t))
  (or (gethash name *binary-type-namespace*)
      (if errorp
	  (error "Unable to find binary type named ~S." name)
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

(defgeneric sizeof (type)
  (:documentation "Return the size in octets of the single argument TYPE,
or nil if TYPE is not constant-sized."))

(defmethod sizeof (obj)
  (sizeof (find-binary-type (type-of obj))))
  
(defmethod sizeof ((type symbol))
  (sizeof (find-binary-type type)))

(defgeneric read-binary (type stream &key &allow-other-keys)
  (:documentation "Read an object of binary TYPE from STREAM."))

(defmethod read-binary ((type symbol) stream &rest key-args)
  (apply #'read-binary (find-binary-type type) stream key-args))

(defgeneric write-binary (type stream object &key &allow-other-keys)
  (:documentation "Write an OBJECT of TYPE to STREAM."))

(defmethod write-binary ((type symbol) stream object &rest key-args)
  (apply #'write-binary (find-binary-type type) stream object key-args))

(defclass binary-type ()
  ((name
    :initarg name
    :initform '#:anonymous
    :reader binary-type-name)
   (sizeof
    :initarg sizeof
    :reader sizeof))
  (:documentation "BINARY-TYPE is the base class for binary types."))

;;; ----------------------------------------------------------------
;;;                      Integer Type (Abstract)
;;; ----------------------------------------------------------------

(defclass binary-integer (binary-type)
  ((endian :type endianess
	   :reader binary-integer-endian
	   :initarg endian
	   :initform nil)))

(defmethod print-object ((type binary-integer) stream)
  (if (not *print-readably*)
      (print-unreadable-object (type stream :type t)
	(format stream "~D-BIT~@[ ~A~] INTEGER TYPE: ~A"
		(* 8 (slot-value type 'sizeof))
		(slot-value type 'endian)
		(binary-type-name type)))    
    (call-next-method type stream)))

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

(defclass binary-unsigned (binary-integer) ())

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
					(* 8 i)))))))
      (values unsigned-value
	      (sizeof type)))))
    
;;; ----------------------------------------------------------------
;;;              Twos Complement Signed Integer Types
;;; ----------------------------------------------------------------

(defclass binary-signed (binary-integer) ())

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

(defclass binary-char8 (binary-type) ())

(setf (find-binary-type 'char8)
  (make-instance 'binary-char8
    'name 'char8
    'sizeof 1))

(deftype char8 () 'character)

(defmethod read-binary ((type binary-char8) stream &key &allow-other-keys)
  (values (code-char (read-binary 'u8 stream))
	  1))

(defmethod write-binary ((type binary-char8) stream object &key &allow-other-keys)
  (write-binary 'u8 stream (char-code object)))

;;; ----------------------------------------------------------------
;;;     Padding Type (Implicitly defined and named by integers)
;;; ----------------------------------------------------------------

;;; The padding type of size 3 octets is named by the integer 3, and
;;; so on.

(defmethod sizeof ((type integer)) type)

(defmethod read-binary ((type integer) stream &key &allow-other-keys)
  (dotimes (i type)
    (read-binary 'u8 stream))
  (values nil type))

(defvar *padding-byte* #x00
  "The value written to padding octets.")

(defmethod write-binary ((type integer) stream object &key &allow-other-keys)
  (declare (ignore object))
  (check-type *padding-byte* (unsigned-byte 8))
  (dotimes (i type)
    (write-binary 'u8 stream *padding-byte*))
  type)

;;; ----------------------------------------------------------------
;;;                   String library functions
;;; ----------------------------------------------------------------

(defun read-binary-string (stream &key size terminators)
  "Read a string from STREAM, terminated by any member of the list TERMINATORS.
If SIZE is provided and non-nil, exactly SIZE octets are read, but the returned
string is still terminated by TERMINATORS. The string and the number of octets
read are returned."
  (check-type size (or null (integer 0 *)))
  (check-type terminators list)
  (assert (or size terminators) (size terminators)
    "Can't read a binary-string without a size limitation nor terminating bytes.")
  (let (bytes-read)
    (values (with-output-to-string (string)
	      (loop with string-terminated = nil
		  for count upfrom 0
		  until (if size (= count size) string-terminated)
		  do (let ((byte (funcall *binary-read-byte* stream)))
		       (cond
			((member byte terminators :test #'=)
			 (setf string-terminated t))
			((not string-terminated)
			 (write-char (code-char byte) string))))
		  finally (setf bytes-read count)))
	    bytes-read)))

;;; ----------------------------------------------------------------
;;;                  String Types
;;; ----------------------------------------------------------------

(defclass binary-string (binary-type)
  ((terminators
    :initarg terminators
    :reader binary-string-terminators)))

(defmacro define-binary-string (type-name size &key terminators)
  (check-type size (integer 1 *))
  `(progn
     (deftype ,type-name () 'string)
     (setf (find-binary-type ',type-name)
       (make-instance 'binary-string
	 'name ',type-name
	 'sizeof ,size
	 'terminators ,terminators))
     ',type-name))

(defmacro define-null-terminated-string (type-name size)
  `(define-binary-string ,type-name ,size :terminators '(0)))

(defmacro define-fixed-size-nt-string (type-name size)
  ;; compatibility..
  `(define-null-terminated-string ,type-name ,size))

(defmethod read-binary ((type binary-string) stream &key &allow-other-keys)
  (read-binary-string stream
		      :size (sizeof type)
		      :terminators (binary-string-terminators type)))

(defmethod write-binary ((type binary-string) stream obj  &key &allow-other-keys)
  (check-type obj string)
  (dotimes (i (sizeof type))
    (if (< i (length obj))
	(funcall *binary-write-byte* (char-code (aref obj i)) stream)
      (funcall *binary-write-byte*
	       ;; use the first member of TERMINATORS as writing terminator.
	       (or (first (binary-string-terminators type)) 0)
	       stream)))
  (sizeof type))

;;; ----------------------------------------------------------------
;;;                    Record Types ("structs")
;;; ----------------------------------------------------------------

;;;(defstruct compound-slot
;;;  name
;;;  type
;;;  on-write)

(defun make-record-slot (&key name type on-write)
  (list name type on-write))

(defun record-slot-name (s) (first s))
(defun record-slot-type (s) (second s))
(defun record-slot-on-write (s) (third s))


(defclass binary-record (binary-type)
  ((slots  :initarg slots
	   :accessor binary-record-slots)
   (offset :initarg offset
	   :reader binary-record-slot-offset)))

(defclass binary-class (binary-record)
  ;; a DEFCLASS class with binary properties
  ((instance-class :type standard-class
		   :initarg instance-class)))

(defmethod binary-record-make-instance ((type binary-class))
  (make-instance (slot-value type 'instance-class)))

(defclass binary-struct (binary-record)
  ;; A DEFSTRUCT type with binary properties
  ((constructor :initarg constructor)))

(defmethod binary-record-make-instance ((type binary-struct))
  (funcall (slot-value type 'constructor)))

(defun slot-offset (type slot-name)
  "Return the offset (in number of octets) of SLOT-NAME in TYPE."
  (unless (typep type 'binary-record)
    (setf type (find-binary-type type)))
  (unless (typep type 'binary-record)
    (error "~S is not of RECORD type."))
  (unless (find-if #'(lambda (slot)
		       (eq slot-name (record-slot-name slot)))
		   (binary-record-slots type))
    (error "Slot ~S doesn't exist in type ~S."
	   slot-name type))
  (+ (binary-record-slot-offset type)
     (loop for slot in (binary-record-slots type)
	 until (eq slot-name (record-slot-name slot))
	 summing (sizeof (record-slot-type slot)))))

(defun binary-record-slot-names (type &optional (padding-slots-p nil))
  "Returns a list of the slot-names of TYPE, in sequence."
  (when (symbolp type)
    (setf type (find-binary-type type)))
  (if padding-slots-p
      (mapcar #'record-slot-name (binary-record-slots type))
    (mapcan #'(lambda (slot)
		(if (integerp (record-slot-type slot))
		    nil
		  (list (record-slot-name slot))))
	    (binary-record-slots type))))

(defun binary-slot-type (type slot-name)
  (when (symbolp type)
    (setf type (find-binary-type type)))
  (let ((slot (find slot-name (binary-record-slots type) :key #'record-slot-name)))
    (assert slot (slot-name)
      "No slot named ~S in binary-type ~S." slot-name type)
    (record-slot-type slot)))

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
   ((eq :label expr) (values 0 nil))	; a label
   ((symbolp expr) (values expr expr))	; a name
   ((integerp expr) (values expr nil))	; a padding type
   ((quoted-name-p expr)
    (values (second expr) (second expr))) ; a quoted name
   ((and (listp expr)			; a nested declaration
	 (symbolp (first expr))
	 (eq (find-package 'binary-types)
	     (symbol-package (first expr))))
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
	       (loop for slot-options on (rest slot-specifier) by #'cddr
		   as slot-option = (first slot-options)
		   with bintype = nil
		   and typetype = nil
		   and on-write = nil
		   if (member slot-option '(:bt-on-write :map-binary-write))
		   do (setf on-write (second slot-options))
		   else if (member slot-option
				   '(:bt :btt :binary-type :binary-lisp-type))
		   do (multiple-value-bind (bt tt nested-form)
			  (parse-bt-spec (second slot-options))
			(setf bintype bt)
			(when nested-form
			  (push nested-form embedded-declarations))
			(when (and (symbolp tt)
				   (member slot-option '(:btt :binary-lisp-type)))
			  (setf typetype tt)))
		   else nconc (list slot-option
				    (second slot-options)) into options
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
					 (list (make-record-slot
						:name slot-name
						:type bintype
						:on-write on-write))
				       nil)))
			       slots))
	     (slot-types (mapcar #'record-slot-type binslots))
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
	   (let ((record-size (loop for s in ',slot-types summing (sizeof s))))
	     (setf (find-binary-type ',type-name)
	       (make-instance 'binary-class
		 'name ',type-name
		 'sizeof record-size
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
		     if (member (first descr)
				'(:bt :btt :binary-type :binary-lisp-type))
		     do (multiple-value-bind (bt lisp-type nested-form)
			    (parse-bt-spec (second descr))
			  (declare (ignore lisp-type))
			  (setf bintype bt)
			  (when nested-form
			    (push nested-form embedded-declarations))
			  (when (and (symbolp bt)
				     (member (first descr)
					     '(:btt :binary-lisp-type)))
			    (setf typetype bintype)))
		     else nconc
			  (list (first descr) (second descr)) into descriptions
		     finally
		       (return (values (list* (first slot-description)
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
					 (list (make-record-slot :name slot-name
								 :type bintype))
				       nil)))
				 slot-descriptions))
	       (slot-types (mapcar #'record-slot-type binslots)))
	  `(progn
	     ,@embedded-declarations
	     (defstruct ,name-and-options
	       ,@doc
	       ,@(mapcar #'parse-slot-description slot-descriptions))
	     (setf (find-binary-type ',type-name)
	       (make-instance 'binary-struct
		 'name ',type-name
		 'sizeof (loop for s in ',slot-types sum (sizeof s))
		 'slots ',binslots
		 'offset 0
		 'constructor (find-symbol (format nil "~A-~A" '#:make ',type-name))))
	     ',type-name))))))


(defmethod read-binary ((type binary-record) stream &key start stop &allow-other-keys)
  (let ((start-slot 0)
	(stop-slot nil))
    (when start
      (setf start-slot (position-if #'(lambda (sp)
					(eq start (record-slot-name sp)))
				    (binary-record-slots type)))
      (unless start-slot
	(error "start-slot ~S not found in type ~A"
	       start type)))
    (when stop
      (setf stop-slot (position-if #'(lambda (sp)
				       (eq stop (record-slot-name sp)))
				   (binary-record-slots type)))
      (unless stop-slot
	(error "stop-slot ~S not found in type ~A"
	       stop  type)))
    (let ((read-bytes 0)
	  (slot-list (subseq (binary-record-slots type) start-slot stop-slot))
	  (obj (binary-record-make-instance type)))
      (dolist (slot slot-list)
	(multiple-value-bind (slot-obj slot-bytes)
	    (read-binary (record-slot-type slot) stream)
	  (setf (slot-value obj (record-slot-name slot)) slot-obj)
	  (incf read-bytes slot-bytes)))
      (values obj read-bytes))))


(defmethod write-binary-record (object stream)
  (write-binary (find-binary-type (type-of object)) stream object))

(defun binary-slot-value (object slot-name)
  "Return the ``binary'' value of a slot, i.e the value mapped
by any MAP-ON-WRITE slot mapper function."
  (let ((slot (assoc slot-name (binary-record-slots (find-binary-type (type-of object))))))
    (assert slot (slot)
      "Slot-name ~A not found in ~S of type ~S."
      slot-name object (find-binary-type (type-of object)))
    (cond
     ((integerp (record-slot-type slot)) nil) ; padding
     ((record-slot-on-write slot)
      (funcall (record-slot-on-write slot)
	       (slot-value object slot-name)))
     (t (slot-value object slot-name)))))

(defmethod write-binary ((type binary-record) stream object
			 &key start stop &allow-other-keys)
  (let ((start-slot 0)
	(stop-slot nil))
    (when start
      (setf start-slot (position-if #'(lambda (sp)
					(eq start (record-slot-name sp)))
				    (binary-record-slots type)))
      (unless start-slot
	(error "start-slot ~S not found in type ~A"
	       start type)))
    (when stop
      (setf stop-slot (position-if #'(lambda (sp)
				       (eq stop (record-slot-name sp)))
				   (binary-record-slots type)))
      (unless stop-slot
	(error "stop-slot ~S not found in type ~A"
	       stop type)))
    (let ((written-bytes 0)
	  (slot-list (subseq (binary-record-slots type) start-slot stop-slot)))
      (dolist (slot slot-list)
	(let* ((slot-name (record-slot-name slot))
	       (slot-type (record-slot-type slot))
	       (value (cond
		       ((integerp slot-type) nil) ; padding
		       ((record-slot-on-write slot)
			(funcall (record-slot-on-write slot)
				 (slot-value object slot-name)))
		       (t (slot-value object slot-name)))))
	  (incf written-bytes
		(write-binary slot-type stream value))))
      written-bytes)))

(defun merge-binary-records (obj1 obj2)
  "Returns a record where every non-bound slot in obj1 is replaced
with that slot's value from obj2."
  (let ((class (class-of obj1)))
    (unless (eq class (class-of obj2))
      (error "cannot merge incompatible records ~S and ~S" obj1 obj2))
    (let ((new-obj (make-instance class)))
      (dolist (slot (binary-record-slots (find-binary-type (type-of obj1))))
	(let ((slot-name (record-slot-name slot)))
	  (cond
	   ((slot-boundp obj1 slot-name)
	    (setf (slot-value new-obj slot-name)
	      (slot-value obj1 slot-name)))
	   ((slot-boundp obj2 slot-name)
	    (setf (slot-value new-obj slot-name)
	      (slot-value obj2 slot-name))))))
      new-obj)))

(defun binary-record-alist (obj)
  "Returns an assoc-list representation of (the slots of) a binary
record object."
  (mapcan #'(lambda (slot)
	      (unless (integerp (record-slot-type slot))
		(list (cons (record-slot-name slot)
			    (if (slot-boundp obj (record-slot-name slot))
				(slot-value obj (record-slot-name slot))
			      'unbound-slot)))))
	  (binary-record-slots (find-binary-type (type-of obj)))))

;;; ----------------------------------------------------------------
;;; Bitfield Types
;;; ----------------------------------------------------------------

(defclass bitfield (binary-type)
  ((storage-type
    :type t
    :accessor storage-type
    :initarg storage-type)
   (hash
    :type hash-table
    :initform (make-hash-table :test #'eq)
    :accessor bitfield-hash)))

(defstruct bitfield-entry
  value
  bytespec)

(defmacro define-bitfield (type-name (storage-type) spec)
  (let ((slot-list			; (slot-name value byte-size byte-pos)
	 (mapcan #'(lambda (set)
		     (ecase (caar set)
		       (:bits
			(mapcar #'(lambda (slot)
				    (list (car slot)
					  1
					  1
					  (cdr slot)))
				(make-pairs (cdr set))))
		       (:enum
			(destructuring-bind (&key byte)
			    (rest (car set))
			  (mapcar #'(lambda (slot)
				      (list (car slot)
					    (cdr slot)
					    (first byte)
					    (second byte)))
				  (make-pairs (cdr set)))))
		       (:numeric
			(let ((s (car set)))
			  (list (list (second s)
				      nil
				      (third s)
				      (fourth s)))))))
		 spec)))
    `(let ((type-obj (make-instance 'bitfield 
		       'name ',type-name
		       'sizeof (sizeof ',storage-type)
		       'storage-type (find-binary-type ',storage-type))))
       (deftype ,type-name () '(or list symbol))
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
;;;;; Consistency check by symmetry. Uncomment for debugging.
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

;;;; Macros:

(defmacro with-binary-file ((stream-var path &rest key-args) &body body)
  "This is a thin wrapper around WITH-OPEN-FILE, that tries to set the
stream's element-type to that required by READ-BINARY and WRITE-BINARY.
A run-time assertion on the stream's actual element type is performed,
unless you disable this feature by setting the keyword option :check-stream
to nil."
  (let ((check-stream (getf key-args :check-stream t))
	(fwd-key-args (copy-list key-args))
	)
    ;; This is manual parsing of keyword arguments. We force :element-type
    ;; to (unsigned-byte 8), and remove :check-stream from the arguments
    ;; passed on to WITH-OPEN-FILE.
    (remf fwd-key-args :check-stream)
    ;; #-(and allegro-version>= (version>= 6 0))
    (setf (getf fwd-key-args :element-type) ''(unsigned-byte 8))
    `(with-open-file (,stream-var ,path ,@fwd-key-args)
      #-cormanlisp
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
collect the individual 8-bit bytes in a list (of integers).
This list is returned by the form. (There is no way to get at
the return-value of BODY.)
This macro depends on the binding of *BINARY-WRITE-BYTE*, which should
not be shadowed."
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
		   (funcall ,save-bwt-var ; it's not our stream, so pass it ...
			    ,closure-byte-var ; along to the next function.
			    ,closure-stream-var)))))
       ,@body
       (car ,stream-var))))

(defmacro with-binary-input-from-list ((stream-var list-form) &body body)
  "Bind STREAM-VAR to an object that, when passed to READ-BINARY, provides
8-bit bytes from LIST-FORM, which must yield a list.
Binds *BINARY-READ-BYTE* appropriately. This macro will break if this
binding is shadowed."
  (let ((save-brb-var (make-symbol "save-brb")))
    `(let* ((,save-brb-var *binary-read-byte*)
	    (,stream-var (cons ,list-form nil)) ; use cell as stream id.
	    (*binary-read-byte* #'(lambda (s)
				    (if (eq s ,stream-var)
					(if (null (car s))
					    (error "WITH-BINARY-INPUT-FROM-LIST reached end of list.")
					  (pop (car s)))
				      (funcall ,save-brb-var s)))))
       ,@body)))

(defmacro with-binary-input-from-vector
    ((stream-var vector-form &key (start 0)) &body body)
  "Bind STREAM-VAR to an object that, when passed to READ-BINARY, provides
8-bit bytes from VECTOR-FORM, which must yield a vector.
Binds *BINARY-READ-BYTE* appropriately. This macro will break if this
binding is shadowed."
  (let ((save-brb-var (make-symbol "save-brb")))
    `(let* ((,save-brb-var *binary-read-byte*)
	    (,stream-var (cons (1- ,start) ,vector-form))
	    (*binary-read-byte* #'(lambda (s)
				    (if (eq s ,stream-var)
					(aref (cdr s) (incf (car s)))
				      (funcall ,save-brb-var s)))))
       ,@body)))

(defmacro with-binary-output-to-vector
    ((stream-var &optional (vector-or-size-form 0)
      &key (adjustable (and (integerp vector-or-size-form)
			    (zerop vector-or-size-form)))
	   (fill-pointer 0)
	   (element-type ''(unsigned-byte 8))
	   (on-full-array :error))
     &body body)
  "Arrange for STREAM-VAR to collect octets in a vector.
VECTOR-OR-SIZE-FORM is either a form that evaluates to a vector, or an
integer in which case a new vector of that size is created. The vector's
fill-pointer is used as the write-index. If ADJUSTABLE nil (or not provided),
an error will occur if the array is too small. Otherwise, the array will
be adjusted in size, using VECTOR-PUSH-EXTEND. If ADJUSTABLE is an integer,
that value will be passed as the EXTENSION argument to VECTOR-PUSH-EXTEND.
If VECTOR-OR-SIZE-FORM is an integer, the created vector is returned,
otherwise the value of BODY."
  (let ((vector-form
	 (if (integerp vector-or-size-form)
	     `(make-array ,vector-or-size-form
			  :element-type ,element-type
			  :adjustable ,(and adjustable t)
			  :fill-pointer ,fill-pointer)
	   vector-or-size-form)))
    (let ((save-bwb-var (make-symbol "save-bwb")))
      `(let* ((,save-bwb-var *binary-write-byte*)
	      (,stream-var ,vector-form)
	      (*binary-write-byte*
	       #'(lambda (byte stream)
		   (if (eq stream ,stream-var)
		       ,(cond
			 (adjustable
			  `(vector-push-extend byte stream
					       ,@(when (integerp adjustable)
						   (list adjustable))))
			 ((eq on-full-array :error)
			  `(assert (vector-push byte stream) (stream)
			     "Binary output vector is full when writing byte value ~S: ~S"
			     byte stream))
			 ((eq on-full-array :ignore)
			  `(vector-push byte stream))
			 (t (error "Unknown ON-FULL-ARRAY argument ~S, must be one of :ERROR, :IGNORE."
				   on-full-array)))
		     (funcall ,save-bwb-var byte stream)))))
	 ,@body
	 ,@(when (integerp vector-or-size-form)
	     (list stream-var))))))
	     
