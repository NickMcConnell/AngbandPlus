;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 200120001999,
;;;;    Department of Computer Science, University of Tromsø, Norway
;;;; 
;;;; Filename:      example.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Wed Dec  8 15:15:06 1999
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: example.lisp,v 1.3 2001/07/12 16:10:55 stig Exp $
;;;;                
;;;;------------------------------------------------------------------

(defpackage "EXAMPLE"
  (:use "COMMON-LISP" "BINARY-TYPES")
  (:export run))

(in-package "EXAMPLE")

;;; ELF basic types
(define-unsigned word 4)
(define-signed sword  4)
(define-unsigned addr 4)
(define-unsigned off  4)
(define-unsigned half 2)

;;; Mapping from ELF symbols to BT:*ENDIAN* values
(defun elf-data-to-endian (elf-data)
  (ecase elf-data
    ((elf-data-2lsb) :little-endian)
    ((elf-data-2msb) :big-endian)))

(defconstant +ELF-MAGIC+ '(#x7f #\E #\L #\F))

;;; ELF file header structure
(define-binary-class elf-header ()
  ((e-ident
    :binary-type (define-binary-struct e-ident ()
		   (ei-magic nil :binary-type
			     (define-binary-struct ei-magic ()
			       (ei-mag0 0 :binary-type u8)
			       (ei-mag1 #\null :binary-type char8)
			       (ei-mag2 #\null :binary-type char8)
			       (ei-mag3 #\null :binary-type char8)))
		   (ei-class nil :binary-type
			     (define-enum ei-class (u8)
			       elf-class-none 0
			       elf-class-32   1
			       elf-class-64   2))
		   (ei-data nil :binary-type
			    (define-enum ei-data (u8)
			      elf-data-none 0
			      elf-data-2lsb 1
			      elf-data-2msb 2))
		   (ei-version 0 :binary-type u8)
		   (padding nil :binary-type 1)
		   (ei-name "" :binary-type
			    (define-null-terminated-string ei-name 8))))
   (e-type
    :binary-type (define-enum e-type (half)
		   et-none 0
		   et-rel  1
		   et-exec 2
		   et-dyn  3
		   et-core 4
		   et-loproc #xff00
		   et-hiproc #xffff))
   (e-machine
    :binary-type (define-enum e-machine (half)
		   em-none  0
		   em-m32   1
		   em-sparc 2
		   em-386   3
		   em-68k   4
		   em-88k   5
		   em-860   7
		   em-mips  8))
   (e-version   :binary-type word)
   (e-entry     :binary-type addr)
   (e-phoff     :binary-type off)
   (e-shoff     :binary-type off)
   (e-flags     :binary-type word)
   (e-ehsize    :binary-type half)
   (e-phentsize :binary-type half)
   (e-phnum     :binary-type half)
   (e-shentsize :binary-type half)
   (e-shnum     :binary-type half)
   (e-shstrndx  :binary-type half)))

(define-condition elf32-reader-error (error)
  ((stream :initarg :stream :reader elf32-parse-error-stream)
   (message :initarg :message :reader elf32-parse-error-message))
  (:report (lambda (condition stream)
	     (princ (elf32-parse-error-message condition)
		    stream))))

(define-condition elf32-wrong-magic (elf32-reader-error)
  ((magic :initarg :magic :reader elf32-wrong-magic-magic)))

(define-condition elf32-wrong-class (elf32-reader-error)
  ((class :initarg :class :reader elf32-wrong-class-class)))

(defun read-elf-file-header (stream)
  "Returns an ELF-HEADER and the file's endianess."
  (let ((header (read-binary 'elf-header stream :stop 'e-type)))
    (with-slots (ei-data ei-class ei-magic) 
        (slot-value header 'e-ident)
      (let* ((binary-types:*endian* (elf-data-to-endian ei-data))
	     (magic (mapcar #'(lambda (slot-name)
				(slot-value ei-magic slot-name))
			    (binary-record-slot-names 'ei-magic))))
	;; Check that file is in fact 32-bit ELF
        (unless (equal +ELF-MAGIC+ magic)
	  (error 'elf32-wrong-magic
		 :stream stream
		 :message (format nil "file doesn't match ELF-MAGIC: ~A" magic)
		 :magic magic))
        (unless (eq 'elf-class-32 ei-class)
          (error 'elf32-wrong-class
		 :stream stream
		 :message (format nil "file is not 32-bit ELF (~A)" ei-class)
		 :class ei-class))
	;; Read the rest of the file-header and merge it with what
	;; we've allready got.
	(let ((rest (read-binary 'elf-header stream :start 'e-type)))
	  (dolist (slot-name (binary-record-slot-names 'elf-header))
	    (unless (slot-boundp header slot-name)
	      (setf (slot-value header slot-name)
		(slot-value rest slot-name))))
	  (values header binary-types:*endian*))))))

(defun run (path)
  (with-binary-file (stream path :direction :input)
    (let ((elf-header (read-elf-file-header stream)))
      (format t "~&ELF header for \"~A\":~:{~&~12@A: ~S~}~%" path
	      (mapcar #'(lambda (slot-name)
			  (list slot-name
				(slot-value elf-header slot-name)))
		      (binary-record-slot-names 'elf-header)))
      elf-header)))

#+unix
(run "/bin/ls")



