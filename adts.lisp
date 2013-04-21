;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: adts.lisp - Various ADTs
Copyright (c) 2003 - Stig Erik Sandø
 (tweaked from various public domain sources)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.datastructures)

(define-condition empty (error)
  ((type :reader empty-type :initform "datastructure" :initarg :type))
  (:report (lambda (condition stream)
	     (format stream "You can't get useful data from an empty ~a"
		     (empty-type condition)))))


(defstruct pq-elem
  "Packing the elements in the priority-queue with it's priority"
  (priority 0)
  value)


(defstruct (heap (:constructor make-heap-struct))
  array
  (size 0)		; how many elements in array is in use and also the index 
                        ; to the last used position
  total-size	       	; (1- (length array)) position 0 is not used
  (adjust-factor 2)	; how much larger array becomes when it is full
  gte                   ; how to compare the keys of two elements 
  key)                  ; how to get the key of an element

;;; INVARIANT:  (gte (key (aref array x))
;;;                  (key (aref array (left x)))))
;;; and likewise with right instead of left

(defun make-heap (&key (gte #'>=)(key #'identity)(size 1)(adjust-factor 2))
  "Returns a new heap"
  (when (<= adjust-factor 1)(error "adjust-factor must be grater than 1"))
  (make-heap-struct :array (make-array (1+ size) :initial-element nil :adjustable t)
		    :size 0
		    :total-size size
		    :gte gte
		    :key key
		    :adjust-factor adjust-factor))
    
(defun parent (index)
  "The position of the parrent to the element in position index"
  (floor index 2))

(defun left (index)
  "The position to the left child to the element in position index"
  (* index 2))

(defun right (index)
  "The position to the left child to the element in position index"
  (1+ (left index)))

;; this one was reimplemented to fix a bug
(defun sift-down (array from size gte key)
  "If the heap invariant is not ok at position from, that value is (repeatetly) swaped with it's largest 'child' (left or right)."
  (declare (ignore gte))
  (let ((child (left from)) ;; * 2
	(par from))
    
    ;; the code should use GTE here, and not hardcoded < >
    (loop
     (when (> child size) (return))

     (when (< (funcall key (aref array child)) (funcall key (aref array (1+ child))))
       (incf child))

     (cond ((> (funcall key (aref array child)) (funcall key (aref array par)))
	    (rotatef (aref array child) (aref array par))
	    (setf par child)
	    (setf child (* 2 child)))
	   (t
	    (setf child (1+ size))))) ;; end loop

    t))


(defun sift-up (new array from gte key)
  "new is placed in position from and as long as it's parrent is smaler they are swaped"
  (let ((new-key (funcall key new)))
    (do ((par (parent from)(parent par)))
	((or (zerop par)(funcall gte (funcall key (aref array par)) 
				 new-key)))
      (setf (aref array from)(aref array par))
      (setq from par))
    (setf (aref array from) new)))

(defun heap-insert (elem heap)
  "Inserts elem in heap, returns size of heap"
  (when (= (heap-size heap)(heap-total-size heap))
    (let* ((old-total-size (heap-total-size heap))
	   (new-total-size (ceiling (* old-total-size (heap-adjust-factor heap)))))
      (setf (heap-array heap)
	(adjust-array (heap-array heap) 
		      (1+ (setf (heap-total-size heap) new-total-size))))))
  
  (assert (pq-elem-p elem))
  (sift-up elem (heap-array heap) (incf (heap-size heap)) (heap-gte heap) (heap-key heap))
  (heap-size heap))

(defun heap-front (heap)
  "Returns the first/largest element in heap"
  (unless (plusp (heap-size heap))(error 'empty :type "heap"))
  (aref (heap-array heap) 1))

(defun heap-remove (heap)
  "Removes the first/largest element in heap"
  (let* ((array (heap-array heap))
	 (size (heap-size heap))
	 (retval (aref array 1))) ;; want the old value returned
    (unless (plusp size) (error 'empty :type "heap"))
    (setf (aref array 1) (aref array size)
	  (heap-size heap) (decf size))
	  
    (sift-down array 1 size (heap-gte heap)
	       (heap-key heap))

    retval))

;;(trace heap-remove)
(defvar *pq-elem-pool* nil)
  
(defun make-priority-queue (&key (gte #'>=) (size 1) (adjust-factor 2))
  "Returns new priority-queue"
  (make-heap :gte gte 
	     :key #'pq-elem-priority 
	     :size size 
	     :adjust-factor adjust-factor))

(defun pq-front (pq)
  "Returns the first element (largest priority) of pq"
  (pq-elem-value (heap-front pq)))

(defun pq-remove (pq)
  "Removing and returning the first element (largest priority) of pq"
  (prog1 (pq-front pq)
    (let ((val (heap-remove pq)))
      (setf (pq-elem-value val) nil)
      (vector-push val *pq-elem-pool*))))

(defun %get-new-pq-elem (priority elem)
  (let ((elm (if (plusp (length *pq-elem-pool*))
		 (vector-pop *pq-elem-pool*)
		 (make-pq-elem))))
    (setf (pq-elem-priority elm) priority
	  (pq-elem-value elm) elem)
    elm))

(defun init-pq-pool (num)
  (setf *pq-elem-pool* (make-array num :fill-pointer t))
  (let ((table *pq-elem-pool*))
    (dotimes (i num)
      (setf (aref table i) (make-pq-elem))
      )))

(defun pq-insert (elem priority pq)
  "Inserting elem in pq with _priority_ priority"
  (heap-insert (%get-new-pq-elem priority elem) pq))

(defun pq-size (pq)
  "Returns the size of the queue."
  (heap-size pq))

(defun heap-build (heap)
  "Reestablishes the heap-invarient"
  (let ((array (heap-array heap))
	(size (heap-size heap))
	(gte (heap-gte heap))
	(key (heap-key heap)))
    (do ((i (parent size)(1- i)))
	((<= i 0))
      (sift-down array i size gte key))))

(defun heap-sort (heap)
  "Sorts the array in heap using heap-sort and returns the array"
  (let ((array (heap-array heap))
	(gte (heap-gte heap))
	(key (heap-key heap)))
    (heap-build heap)
    (do ((i (heap-size heap) (1- i)))
	((<= i 1))
      (sift-down array 1 i gte key)
      (rotatef (aref array 1)(aref array i)))
    array))


;;; an ordinary queue

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
;;; end queue-code
