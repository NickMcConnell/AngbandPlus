(in-package :cl-user)

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


(defun read-graf-file (fname)

  (let ((floors '())
	(kinds '())
	(monsters '())
	)
  
    (with-open-file (in-str (pathname fname)
			    :direction :input)
    
      (loop for l = (read-line in-str nil 'eof)
	    until (eq l 'eof)
	    do
	    (let ((first-char (if (> (length l) 0)
				  (schar l 0)
				  nil)))
	    
	      (case first-char
		((#\# nil #\Space)
		 ;; ignore
		 nil)
		((#\f #\F)
		 ;;(warn "Floor ~s" l)
		 (let ((res (split-seq-on l #\:)))
		   (push (list (second res) (third res) (fourth res)) floors))
		 )
	      
		((#\r #\R)
		 (let ((res (split-seq-on l #\:)))
		   (push (list (second res) (third res) (fourth res)) monsters))
		 ;;	       (warn "Monster ~s" l)
		 )

		((#\s #\S)
		 ;; spell
		 )

		((#\k #\K)
		 ;; kind
		 (let ((res (split-seq-on l #\:)))
		   (push (list (second res) (third res) (fourth res)) kinds))
		 )
		(#\%
		 ;; directive, ignore
		 )
	      
		(otherwise
		 (warn "Unhandled char ~s" first-char)
		 ))

	      )))

    (with-open-file (out-str (pathname "graf-prefs.lisp")
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create)
      (let ((*print-case* :downcase))

	(pprint '(in-package :org.langband.vanilla)
		out-str)
      
	(dolist (i (reverse floors))
	  (let ((num (parse-integer (first i)))
		(attr (second i))
		(char (third i)))
	    (setf (char attr 0) #\#)
	    (setf (char char 0) #\#)

	    (setf attr (read-from-string attr)
		  char (read-from-string char))

	    (pprint `(update-floor-display ,num ,attr ,char)
		    out-str)
	    ;;(warn "Floor ~s ~s ~s" num attr char)
	    ))

	(terpri out-str)
    
	(dolist (i (reverse kinds))
	  (let ((num (parse-integer (first i)))
		(attr (second i))
		(char (third i)))
	    (setf (char attr 0) #\#)
	    (setf (char char 0) #\#)
	
	    (setf attr (read-from-string attr)
		  char (read-from-string char))

	    (pprint `(update-kind-display ,num ,attr ,char)
		    out-str)
	    ))

	(terpri out-str)
    
	(dolist (i (reverse monsters))
	  (let ((num (parse-integer (first i)))
		(attr (second i))
		(char (third i)))
	    (setf (char attr 0) #\#)
	    (setf (char char 0) #\#)
	
	    (setf attr (read-from-string attr)
		  char (read-from-string char))

	    (pprint `(update-monster-display ,num ,attr ,char)
		    out-str)
	    ))

	
	t))
  
    t))

(read-graf-file "graf-dvg.prf")

#+cmu
(when ext:*batch-mode*
  (cl-user::quit))

