;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: base.lisp - basic code for the rest of the game
Copyright (c) 2000-2003 - Stig Erik Sandø

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
  (deftype u16b () '(unsigned-byte 16))
  (deftype u24b () '(unsigned-byte 24))
  (deftype u32b () '(unsigned-byte 32))

  ;;  (deftype vinfo-bit-type () `(unsigned-byte 32))
  (deftype vinfo-bit-type () `(unsigned-byte 16))

  (deftype =char-code= ()
    #+handle-char-as-num
    'u16b
    #-handle-char-as-num
    'character
    )

  ;; the types that return-action of an event may have.
  (deftype return-actions ()
    `(member :remove-event :keep-event))

  (deftype event-types ()
    '(member :on-create :on-pre-equip :on-post-equip :step-on-coord))
  )

  
(define-condition langband-quit (condition) ()) 
(define-condition savefile-problem (serious-condition)
  ((desc :initarg :desc :reader saveproblem.desc)))

;;; === Some binary types
(bt:define-unsigned u64 8)
(bt:define-signed s64 8)
(bt:define-unsigned u128 16)

;;; === Some macros we need right away

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

;;; === End important/general macros

;;; === Some dynamic variables of importance for the rest of the system:

;;(defvar *game-parameters* (make-hash-table :test #'eq)
;;  "a table with keyword game-parameters")

;; four very important variables :-)
(defvar *variant* nil "variant in use.  one should not rebind this
too frequently.")
(defvar *level* nil "The current level, for good and bad.")
(defvar *dungeon* nil "global dungeon object")
(defvar *player* nil "the player object")


(defcustom *redraw* u-fixnum 0 "what to redraw, bitfield")
(defcustom *update* u-fixnum 0 "what to update, bitfield")

(defvar *cur-dun* nil
  "a dynamic variable which is set to an object
of type DUN-DATA (see: dungeon.lisp) and is valid
throughout dungeon-generation")

(defvar *hitpoint-warning* 3
  "Value in [0..9] of when to warn about hitpoint-losses")

;; must change
(defvar *cur-win* nil "Pointer to the currently active window.
Should not be altered directly.")

(defvar *input-event* nil "An input-event that can be reused to avoid
consing up a new one.")


(defvar *obj-type-mappings* (make-hash-table :test #'eq)
  "keeps track of mapping from key to object-types, used by factories.")

(defvar *engine-version* "0.1.4" "A version specifier that can be used for
display and listings, not useful for internal code.")
(defvar *engine-num-version* 123 "A numeric version for the engine that can
be used by internal code.  It will typically be incremented with every
non-compatible change, so when connecting a variant to the engine, this is
the number you should look at. It's quick to compare against and it's unambigious.")

(defvar *engine-source-dir* "./")
(defvar *engine-data-dir* "./data/")
(defvar *engine-config-dir* "./config/")

;; must be set to T by init for use of graphics.
(defvar *graphics-supported* nil)

(defvar *dumps-directory* "doc/dumps/" "Where should various debug-dumps go?")

;;; === End dynamic variables

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

;; turn into a deftype later
(defun nonboolsym? (sym)
  (and sym (not (eq sym t)) (symbolp sym)))

(defun symbolify (data)
  "Returns a symbol in a form which can be understood when reading code."
  (if (eq data nil)
      nil
      `',data))

(defsubst i2a (num)
  "Returns the letter corresponding to #\a + num."
  (declare (type u-fixnum num))
  (code-char (with-type u-fixnum (+ #.(char-code #\a) num))))

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
    

#+compiler-that-inlines
(defsubst int-/ (a b)
;;  (declare (type u-fixnum a b))
;;  (the u-fixnum
    (prog1 (floor a b))
;;    )
  )


#-compiler-that-inlines
(defmacro int-/ (a b)
  "Integer division, as in C."
  `(prog1 (floor ,a ,b)))



(defun shrink-array! (arr)
  "Shrinks the array and removes NIL gaps. Returns the new size."

  (let ((len (length arr))
	(cur-write 0)
	(cur-obj nil))
    (declare (type u-fixnum cur-write len))
    
    (loop for cur-read of-type fixnum from 0 below len
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


(defun shuffle-array! (tmp-arr len)
  "Shuffles the given array"
  (declare (type u-fixnum len))
  
  (loop for i of-type u-fixnum from 0 below len
	for rnd-val = (+ i (random (- len i)))
	do
	(rotatef (aref tmp-arr i) (aref tmp-arr rnd-val)))
  
  tmp-arr)

(defun get-array-with-numbers (len &key fill-pointer)
  "Returns an array with increasing numbers."
  (let ((arr (if fill-pointer
		 (make-array len :fill-pointer t)
		 (make-array len))))
    
    (loop for i from 0 below len
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
  
  (if (and (plusp number) (plusp dice))
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

(defun verify-id (id)
  "Verifies the id, returns NIL on failure, T when ok."
  (flet ((char-checker (x)
	   (cond ((eql x #\-)
		  t)
		 ((eql x #\/)
		  t)
		 ((eql x #\*)
		  t)
		 ((alpha-char-p x) ;; fix to only lowercase later
		  t)
		 ;; a temporary one, remove later
		 ((digit-char-p x)
		  t)
		 (t nil))))
    (if (stringp id)
	(every #'char-checker id)
	nil)))
		      
   

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
(let ((counter 0))
  (defun %dump-profile-to-file ()
    (let ((pname (concatenate 'string *dumps-directory* "prof." (format nil "~a" (incf counter)) ".dump")))
      (with-open-file (s (pathname pname)
			 :direction :output
			 :if-exists :supersede)
	(prof:show-flat-profile :stream s :verbose t)
	(prof:show-call-graph :stream s :verbose t)
	))))

#+allegro
(defmacro tricky-profile (expr type)
  `(prof:with-profiling (:type ,type)
    (prog1
	,expr
      (%dump-profile-to-file))))

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
  #+sbcl (sb-ext:gc)
  #+lispworks (hcl:normal-gc)
  #-(or allegro cmu clisp lispworks sbcl)
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


;; hackish, must be improved later 
(defun text-paint-value (attr char)
  (let ((num-val (etypecase char
		   (character (char-code char))
		   (integer char))))
    (logior (dpb attr (byte 8 8) 0) num-val)))

(defun tile-paint-value (file tile)
  (logior (dpb file (byte 8 24) 0) (dpb tile (byte 16 8) 0)))

#||
(defmacro tile-number (num)
  `(+ +graphics-start+ ,num))

;; also support names!
(defmacro tile-file (num)
  `(+ +graphics-start+ ,num))
||#
#||
(defmacro tile-number (num)
  num)

;; also support names!
(defmacro tile-file (num)
  num)
||#

;; put the tile-number in bits 9-24
(defmacro tile-number (tile)
  `(dpb ,tile (byte 16 8) 0))

;; put the tile-file in bits 25-32
(defmacro tile-file (file)
  `(dpb ,file (byte 8 24) 0))

(defmacro make-legal-attr (attr)
  `(dpb ,attr (byte 8 8) 0))
