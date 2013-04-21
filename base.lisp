;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: base.lisp - basic code for the rest of the game
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; 28 bits
  (deftype u-fixnum () '(unsigned-byte 28))
  ;; 16 bits
  (deftype u-16b () '(unsigned-byte 16))
;;  (deftype vinfo-bit-type () `(unsigned-byte 32))
  (deftype vinfo-bit-type () `(unsigned-byte 16))


  (deftype =char-code= ()
    #+handle-char-as-num
    'u-16b
    #-handle-char-as-num
    'character
    ))
  
(define-condition langband-quit (condition) ()) 
  
(defclass activatable ()
  ((activated :reader activated? :initform nil))
  (:documentation "Mixin-class for activatation of objects,
may be removed later for efficiency-reasons.  It enforces a
protocol that allows activated? to be set automagically after
a succesful ACTIVATE-OBJECT."))
  
  
;; move me later
(defgeneric activate-object (obj &key &allow-other-keys)
  (:documentation "Most objects in Langband is created lazily.
This means that an object may be created but may not be fully initialised
and filled with appropriate values right away.  The normal CL/CLOS mechanisms
deal with the actual creation of the bare object, but non-trivial objects
should also be \"activated\", ie get proper values on all variables.
The object in question must be returned, failure to do so may lead to a
situation where the system assumes the object is invalid."))  

(defgeneric ok-object? (obj)
  (:documentation "Checks to make sure the object is ok."))

(defgeneric convert-obj (obj to &key &allow-other-keys)
  (:documentation "Tries to convert the OBJ to the TO form, in pretty
much the same way as COERCE."))



;; some binary types
(bt:define-unsigned u64 8)
(bt:define-signed s64 8)


(defmethod convert-obj (obj to &key)
  (error "Conversion from ~s to ~s not implemented." obj to)
  ;;(coerce obj to)
  )

(defmethod activate-object (obj &key)

  obj)

(defmethod activate-object :around ((obj activatable) &key)
   (unless (next-method-p)
     ;; this will never happen
     (lang-warn "Unable to find ACTIVATE-OBJECT for type ~a" (type-of obj))
     (return-from activate-object nil))

   ;; we pass along the same arguments.. 
   (let ((result (call-next-method)))
     ;; we only say that an object is activated if it returned the object
     (cond ((eq obj result)
	    (setf (slot-value obj 'activated) t)
	    obj)
	   
	   (t
	    (lang-warn "Activation of object ~a failed, return was ~a" obj result)
	    nil)
	   )))


(defmacro defsubst (name arglist &body body)
  "Declare an inline defun."
  `(progn
    (declaim (inline ,name))
    (defun ,name ,arglist ,@body)))

(defmacro defcustom (name type init doc)
  "Define a typed variable."
  `(progn
    (declaim (type ,type ,name))
    (defvar ,name (the ,type ,init) ,doc)))

(defmacro defconst (name type init doc)
  "Define a typed constant."
  `(progn
    (declaim (type ,type ,name))
    (defconstant ,name (the ,type ,init) ,doc)))

(defmacro with-type (type expr)
  "Evaluate the arithmetic expression in TYPE.
Adopted from P.Graham `ANSI CL', p 410; with some modifications."
  `(the ,type
    ,(if (and (consp expr)
              (member (car expr) '(+ - * / 1+ 1- random abs sin cos tan cot
                                   signum log exp expt)
                      :test #'eq))
         (let ((nexp
                (labels ((binarize (expr)
                           (if (and (nthcdr 3 expr)
                                    (member (car expr) '(+ - * /)))
                               (destructuring-bind (op a1 a2 . rest) expr
                                 (binarize `(,op (,op ,a1 ,a2) ,@rest)))
                               expr)))
                  (binarize expr))))
           `(,(car nexp) ,@(mapcar #'(lambda (ee) `(with-type ,type ,ee))
                                   (cdr nexp))))
         expr)))

(defmacro while (test &body body)
  "repeat BODY while TEST is true"
  `(do ()
       ((not ,test))
     ,@body))

(defmacro when-bind ((var expr) &body body)
  "generalisation of (let ((var expr)) (when var ...))."
  `(let ((,var ,expr))
    (when ,var
      ,@body)))

(defmacro unless-bind ((var expr) &body body)
  "generalisation of (let ((var expr)) (unless var ...))."
  `(let ((,var ,expr))
    (unless ,var
      ,@body)))

(defun split-seq-on (str &optional (ch #\Space))
  "returns a list of strings formed by breaking STR at every occurance
of CH (which is not included).  Works for any sequence, not just strings,
but optimized for vectors."
  (when str
    (do* ((prev-pos 0 (1+ next-pos))
          (next-pos (position ch str)
                    (position ch str :start prev-pos))
          (stuff (list (subseq str 0 next-pos))
                 (cons (subseq str prev-pos next-pos)
                       stuff)))
        ((null next-pos) (nreverse stuff)))))

(defun make-queue ()
  (cons nil nil))

(defun queue-as-list (q)
  (car q))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
	    (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))


(defun register-variant-common& (&key before-game-init after-game-init)
  "Registers callbacks for common functions for all variants. Called
before variant init-functions."
  (setf (get 'common 'pre-init) before-game-init
	(get 'common 'post-init) after-game-init))


(defmacro charify-number (num)
  #+handle-char-as-num
  num
  #-handle-char-as-num
  `(code-char ,num)
  )

(defmacro numberify-char (chr)
  #+handle-char-as-num
  chr
  #-handle-char-as-num
  `(char-code ,chr)
  )



(defun symbolify (data)
  "Returns a symbol in a form which can be understood when reading code."
  (if (eq data nil)
      nil
      `',data))

(defsubst i2a (num)
  "Returns the letter corresponding to #\a + num."
  (declare (type u-fixnum num))
  (code-char (with-type u-fixnum (+ (char-code #\a) num))))

(defsubst a2i (char)
  "Returns the number corresponding to the char given, where #\a is 0."
  (- (char-code char) (char-code #\a)))
#||
(defsubst randint (num)
  "Returns (1+ (random num))."
  (declare (type u-fixnum num))
  (with-type u-fixnum (1+ (random num))))
||#

;; hack ever so long
(defun randint (num)
  (1+ (random num)))

#||
(defsubst rand-range (a b)
  "Returns a random numer in the range [a..b]."
  (declare (type u-fixnum a b))
  (with-type u-fixnum (+ a (random (1+ (- b a))))))
||#

;; hack ever so long
(defun rand-range (a b)
  (+ a (random (1+ (- b a)))))

(defun rand-spread (a b)
  (rand-range (- a b) (+ a b)))

(defun rand-elm (seq)
  "Returns a random element from given sequence."
  (let* ((len (length seq))
	 (elm (random len)))
    (elt seq elm)))
    

#+(or cmu lispworks)
(defsubst int-/ (a b)
;;  (declare (type u-fixnum a b))
;;  (the u-fixnum
    (prog1 (floor a b))
;;    )
  )


#-(or cmu lispworks)
(defmacro int-/ (a b)
  "Integer division, as in C."
  `(prog1 (floor ,a ,b)))



(defun shrink-array! (arr)
  "Shrinks the array and removes NIL gaps. Returns the new size."

  (let ((len (length arr))
	(cur-write 0)
	(cur-obj nil))
    (declare (type u-fixnum cur-write len))
    
    (loop for cur-read of-type fixnum from 0 to (the fixnum (1- len))
	  do
	  (setq cur-obj (aref arr cur-read))
	  (when cur-obj
	    (setf (aref arr cur-write) cur-obj)
	    (incf cur-write)))

    (setf (aref arr cur-write) nil)
    
    cur-write))


(defun add-object-to-array! (arr cur-size max-size aobj)
  "Adds an object to array. Returns T if succesful
and NIL if unsuccesful."
  (declare (type u-fixnum cur-size max-size))
  (cond ((< cur-size max-size)
	 ;; we have room
;;	 (warn "Adding ~a to array" aobj)
	 (setf (aref arr cur-size) aobj)
	 t)
	;; we're full
	(t
	 (lang-warn "equipment full..")
	 nil)))


(defmethod convert-obj ((htbl hash-table) (to (eql :vector)) &key sort-table-p sorted-by-key sorted-by-fun fill-pointer)
  "Takes a hash-table and returns a vector with the elements."
  
  (let* ((len (hash-table-count htbl))
	 (arr (if fill-pointer
		  (make-array len :initial-element nil :fill-pointer t)
		  (make-array len :initial-element nil))))
	 
    (declare (type u-fixnum len))
    
    (loop for i of-type u-fixnum from 0
	  for x being the hash-values of htbl
	  do
	  (setf (aref arr i) x))
    
    (when sort-table-p
      (let ((sort-args (list arr (if sorted-by-fun sorted-by-fun #'<))))
	(when sorted-by-key
	  (setq sort-args (append sort-args (list :key sorted-by-key))))
	(setq arr (apply #'sort sort-args))))
    
    arr))

(defun shuffle-array! (tmp-arr len)
  "Shuffles the given array"
  (declare (type u-fixnum len))
  
  (loop for i of-type u-fixnum from 0 to (1- len)
	for rnd-val = (random len)
	do
	(rotatef (aref tmp-arr i) (aref tmp-arr rnd-val)))
  
  tmp-arr)

(defun get-array-with-numbers (len &key fill-pointer)
  "Returns an array with increasing numbers."
  (let ((arr (if fill-pointer
		 (make-array len :fill-pointer t)
		 (make-array len))))
    
    (loop for i from 0 to (1- len)
	  do
	  (setf (aref arr i) i))
    
    arr))

(defun parse-dice (str)
  "Parses a dice and returns a CONS with num-dice and base-dice."
  (let ((pos (position #\d str)))
    (cons (parse-integer (subseq str 0 pos))
	  (parse-integer (subseq str (1+ pos))))))

(defun roll-dice (number dice)
  "Rolls dice numbber of times and returns the result."
  (declare (type u-fixnum number dice))
  (if (plusp number)
      (loop for x from 1 to number
	    summing (randint dice))
      0))

(defun parse-and-roll-dice (str)
  "Parses and rolls the dice-str."
  (let ((nums (parse-dice str)))
    (roll-dice (car nums) (cdr nums))))


(defmacro bit-flag-add! (loc &rest flags)
  "Same as 'loc |= flags', and uses LOGIOR."
  `(setf ,loc (logior ,loc ,@flags)))

(defmacro bit-flag-remove! (loc flag)
  "Same as 'loc &= ~(flag)', and uses LOGANDC2."
  `(setf ,loc (logandc2 ,loc ,flag)))

;; change me into a macro at some point?
#-compiler-that-inlines
(defmacro bit-flag-set? (loc flag)
  `(/= 0 (logand ,loc ,flag)))

#+compiler-that-inlines
(defun bit-flag-set? (loc flag)
  "Checks if the given flag is set, and returns T or NIL."
  (/= 0 (logand loc flag)))

#-compiler-that-inlines
(defmacro bit-flag-and (pos1 pos2)
  `(/= 0 (logand ,pos1 ,pos2)))

#+compiler-that-inlines
(defun bit-flag-and (pos1 pos2)
  (/= 0 (logand pos1 pos2)))

(defsubst read-one-character ()
  "Reads one character from the C-side."

  #-handle-char-as-num
  (c-inkey!)
  #+handle-char-as-num
  (code-char (c-inkey!))
  )
  
(defsubst clear-the-screen! ()
  "Clears the screen on the C-side."
  (c-term-clear!)
  #+cmu
  (c-clear-from! 0))

(defun compile-in-environment (func)
  (let (
	#+cmu (*compile-print* nil)
	      #+cmu (*load-verbose* nil)
	      (*load-print* nil)
	      ;;#+cmu (*error-output* o-str)
	      #+cmu (extensions:*gc-verbose* nil)
	      )
    (funcall func)))

(defun text-to-ascii (str)
  "converts a c-type string to a lisp-string in ascii."
  
  (let ((backslashed nil)
	(controlled nil))
    
    (with-output-to-string (s)
      (loop for x across str
	  do 
	    ;;(warn "checking ~s" x)
	    (cond (backslashed
		   (case x
		     (#\\ (write-char #\\ s))
		     (#\s (write-char #\Space s))
		     (#\b (write-char #\Backspace s))
		     (#\n (write-char #\Linefeed s))
		     (#\r (write-char #\Return s))
		     (#\t (write-char #\Tab s))
		     (#\e (write-char #\Escape s))
		     ;; skip hex
		     (otherwise
		      (write-char x s)))
		   (setq backslashed nil))

		  (controlled
		   (write-char (code-char (logand (char-code x) #o37)) s)
		   (setq controlled nil))
		  ((eql x #\\) 
		   (setq backslashed t))
		  ((eql x #\^)
		   (setq controlled t))
		  
		  (t
		   (write-char x s))))
      s)))

;;(trace text-to-ascii)

#+allegro
(defmacro tricky-profile (expr type)
  `(prof:with-profiling (:type ,type)
    (prog1
	,expr
      (with-open-file (s (pathname "prof.dump")
			    :direction :output
			    :if-exists :supersede)
	   (prof:show-flat-profile :stream s :verbose t)
	   (prof:show-call-graph :stream s :verbose t)
	   ))))

#-allegro
(defmacro tricky-profile (expr type)
  (declare (ignore type))
  `(time ,expr))

(defun get-late-bind-function (package name)
  "Tries to find a function that may not exist at read, compile
or load time, ie totally dynamic."
  (let* ((pack (find-package package))
         (sym (find-symbol (symbol-name name) pack)))
    (when (fboundp sym)
      (fdefinition sym))))

#+xp-testing
(defun do-a-test (stage)
  (when-bind (func (get-late-bind-function 'lb-test 'run-lb-test))
    (funcall func stage :verbose t)))

(defun garbage-collect (&key (global nil))
  "Tries to enforce a garbage collect."
  (declare (ignore global))
  #+cmu (ext:gc)
  #+allegro (excl:gc t)
  #+clisp (ext:gc)
  #+lispworks (hcl:normal-gc)
  #-(or allegro cmu clisp lispworks)
  (lang-warn "explicit GC not implemented."))

(defun lang-warn (format-string &rest format-args)
  "Prints a warning for Langband-system.  It works almost like
regular WARN except that no condition is sent, use regular WARN for such
cases.  Leaks memory, only use when testing."

  #-clisp
  (format *error-output* "~&~@<LB-Warning:  ~3i~:_~A~:>~%"
          (apply #'format nil format-string format-args))
  ;; ugly
  #+clisp
  (format *error-output* "~&~a~%" (apply #'format nil format-string format-args)))


;; remove these ones later:

(defun-memo %get-6str (num)
  (format nil "~6d" num))

(defun-memo %get-5str (num)
  (format nil "~5d" num))

(defun-memo %get-4str (num)
  (format nil "~4d" num))

(defun-memo %get-8str (num)
  (format nil "~8d" num))

(defun-memo %get-9str (num)
  (format nil "~9d" num))

(defun-memo %get-13astr (val)
  (format nil "~13@a" val))


(defmacro with-foreign-str ((str-var) &body body)
  `(let ((,str-var (org.langband.ffi::%get-fresh-str)))
    (declare (type (array base-char (1024)) ,str-var))
    ,@body))


(defun %wr-str-to-dest (str dest)
  "Writes to given string.  Returns number of chars written."

  (let* ((cur (fill-pointer dest))
	 (len (length str))
	 (dest-len (array-dimension dest 0))
	 (end-pos (+ cur len))
	 (diff (- dest-len end-pos)))

    (when (minusp diff)
      (error "Too long string for destination..")) ;; fix later, an error will do now

    (setf (fill-pointer dest) end-pos)
    
    (loop for c across str
	  for i from cur
	  do
	  (setf (char dest i) c))
    len))

(defun %wr-char-to-dest (chr dest)

;;  (describe dest)
  (let ((cur (fill-pointer dest)))
    (setf (fill-pointer dest) (1+ cur))
    (setf (char dest cur) chr)
    1))

(defun %wr-int-to-dest (int dest)
  "Borrowed and modified from CMUCL."
  (let ((quotient ())
        (remainder ()))
    ;; Recurse until you have all the digits pushed on the stack.
    (if (not (zerop (multiple-value-setq (quotient remainder)
                      (truncate int *print-base*))))
        (%wr-int-to-dest quotient dest))
    ;; Then as each recursive call unwinds, turn the digit (in remainder) 
    ;; into a character and output the character.
    (%wr-char-to-dest (code-char (+ (char-code #\0) remainder)) dest)))


(defun lb-format (dest format-str &rest args)
  "Tries to format things right."

  (flet ((output-int (arg)
	   (let ((*print-base* 10))
	     (cond ((< arg 0)
		    (%wr-char-to-dest #\- dest)
		    (%wr-int-to-dest (- arg) dest))
		   (t					  
		    (%wr-int-to-dest arg dest))))))
	 
    (cond ((not (stringp dest))
	   (apply #'cl:format dest format-str args))
	  (t
	   (let ((last-char #\a)
		 (arg-iter args))
	     (loop for x across format-str
		   do
		   (cond ((eql last-char #\~)
			  (ecase x
			    (#\a (let ((arg (car arg-iter)))
				   (etypecase arg
				     (symbol
				      (when (keywordp arg)
					(%wr-char-to-dest #\: dest))
				      (%wr-str-to-dest (symbol-name arg) dest))
				     (integer
				      (output-int arg))
				    
				     (string
				      (%wr-str-to-dest arg dest))
				     ;;(nil
				     ;; (%wr-str-to-dest "nil" dest))
				     )
				   (setf arg-iter (cdr arg-iter))))
			  
			    (#\d (let ((arg (car arg-iter)))
				   (etypecase arg
				     (integer
				      (output-int arg)))
				   (setf arg-iter (cdr arg-iter))))
			  
			    (#\% (%wr-char-to-dest #\Newline dest))
			    (#\~ (%wr-char-to-dest #\~ dest)))
			  (setf last-char #\a);; dummy
			  )
			 (t
			  (if (eql x #\~)
			      nil
			      (%wr-char-to-dest x dest))
			  (setf last-char x)))
		   )))
	  )))

(declaim (inline mystrcat))
(defun mystrcat (x y)
  "Basically catenates strings and tries to stringify arguments to be sure"
  (concatenate 'string (string x) (string y)))


(defun get-symcase-fun ()
  "Returns the symcase-fun as a symbol."
  #+allegro
  (ecase excl:*current-case-mode*
    (:case-sensitive-lower
     'string-downcase)
    (:case-insensitive-upper
     'string-upcase))
  #-allegro
  'string-upcase)

(defmacro concat-pnames (&rest args) 
  "concatenates strings or symbols and returns an interned
symbol which can be passed to e.g defun (as name of function)."

  (let ((str (gensym))
        (case-fun (get-symcase-fun)))

    `(let ((,str (,case-fun (reduce #'mystrcat (list ,@args)))))
       (if (and (plusp (length ,str)) (eql (char ,str 0) #\:))
           (intern (subseq ,str 1) (find-package :keyword))
           (intern ,str)
           ))
    ))


#||
(defun register-variant& (id name &key before-game-init after-game-init)
  "Registers variant in appropriate places."
  
  (setf (get 'variant 'id) id
	(get 'variant 'name) name
	(get 'variant 'pre-init) before-game-init
	(get 'variant 'post-init) after-game-init))
||#
