;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: conversation.lisp - conversation code
Copyright (c) 2002 - Knut Arild Erstad

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defstruct (conversation-node (:conc-name cnode.))
  (id nil)            ; global id
  (text "")           ; text or a function returning text
  (perform nil)       ; function to perform at entry
  (options '())       ; all possible replies (including hidden ones)
  (skip-test nil)     ; function to test for "skipping" this node
  (skip-dest nil)     ; destination to skip to: node or id string
  )

(defstruct (conversation-option (:conc-name copt.))
  (text "<undefined>")   ; text or a function returning text
  (test nil)             ; optional test closure; hide if it returns false
  (perform nil)          ; function to perform when chosen
  (dest nil)             ; destination node (conversation-node), id of node
			 ; (string), one of several special keywords
			 ; like :QUIT (?), or a function returning one of the
                         ; above
  )

(defstruct (conversation-parameters (:conc-name cparam.))
  (player nil)  ; player character
  (npc nil) ; non-player character
  )

(defparameter *conversations* (make-hash-table :test 'equal))

(defun get-filtered-options (node cparam)
  (loop for opt in (cnode.options node)
	when (or (null (copt.test opt))
		 (funcall (copt.test opt) cparam))
	collect opt))

(defun get-node-text (node cparam)
  (let ((text (cnode.text node)))
    (ctypecase text
      (string text)
      (function (funcall text cparam)))))

(defun get-option-text (opt cparam)
  (let ((text (copt.text opt)))
    (ctypecase text
      (string text)
      (function (funcall text cparam)))))

(defun get-destination (option cparam)
  (let ((dest (copt.dest option)))
    (if (functionp dest)
	(funcall dest cparam)
	dest)))

(defun skip-node? (node cparam)
  ;; eventually like this? (or (and (functionp ...)) (and (stringp ...)))
  (and (functionp (cnode.skip-test node))
       (funcall (cnode.skip-test node) cparam)))

(defun get-conversation-node (id-or-node cparam)
  (assert id-or-node)
  (if (keywordp id-or-node)
      id-or-node
      (let ((node id-or-node)
	    (max-count 10))
	;; this loop is not meant to be pretty :>
	(loop
	 (when (zerop (decf max-count))
	   (error "GET-CONVERSATION-NODE: Probably circular conversation skipping."))
	 (when (stringp node)
	   (setf node (gethash node *conversations*)))
	 (when (keywordp node)
	   (return node))
	 (assert (conversation-node-p node))
	 (if (skip-node? node cparam)
	     (setf node (cnode.skip-dest node))
	     (return node))))))

(defun maybe-perform (fun cparam)
  (when fun
    (funcall fun cparam)))

(defun %conversation (&rest args &key id &allow-other-keys)
  (let ((node (apply #'make-conversation-node args)))
    (when id
      (setf (gethash id *conversations*) node))
    node))

(defun %option (&rest args)
  (apply #'make-conversation-option args))

(defun %quit-option (&optional (text "Bye."))
  (%option :text text :dest :quit))

(defun display-conversation-node (node cparam)
  "Display a single conversation node and its conversation options (replies)."
  (clear-window *cur-win*)
  ;; maybe perform something
  (maybe-perform (cnode.perform node) cparam)
  ;; display text
  (let* ((row-offset (+ (print-text! 6 2 +term-l-blue+ (get-node-text node cparam)) 2))
	 (row row-offset)
	 (picture "people/male-hobbit-rogue.png")
	 (col 3)
	 (i -1)
	 (code-a (char-code #\a))
	 (options (get-filtered-options node cparam)))

    ;; show picture (make it depend on conversation, should also have some checks on size and placement)
    (when (use-images?)
      ;; hackish, improve later
      (when (and picture (stringp picture))
	(let ((pic-col (- (get-frame-width +dialogue-frame+) 20)))
	  (paint-gfx-image& picture pic-col 1))))


    ;; assign characters to filtered options and display them
    (dolist (opt options)
      (incf i)
      (let* ((c (code-char (+ code-a i)))
	     (text (get-option-text opt cparam)))
	;; assume that there are not too many options to fit the screen
	(put-coloured-str! +term-white+ (format nil "~A." c) col row)
	(setf row (1+ (print-text! 6 row +term-l-green+ text)))))
    ;; print prompt
    (put-coloured-str! +term-l-blue+ "-> Reply by pressing a key: " col (1+ row))
    ;; loop until we get a valid key
    (let ((max-code (+ code-a i))
	  (key (read-one-character)))
      (loop until (or (eql key +escape+)
		      (<= code-a (char-code key) max-code))
	    do (setf key (read-one-character)))
      ;; return the chosen option, or nil if escape was pressed
      (if (eql key +escape+)
	  nil
	  (nth (- (char-code key) code-a) options)))))

(defun display-conversation (id cparam)
  (let ((node (get-conversation-node id cparam)))
    (loop
     (let ((option (display-conversation-node node cparam)))
       (when (null option)
	 ;; quit when escape is pressed, the conversations should probably just
	 ;; be designed so this does not create flag problems
	 ;; (maybe change this behavior later, or allow it to be configurable?)
	 (return))
       (assert (conversation-option-p option))
       ;; perform hook
       (maybe-perform (copt.perform option) cparam)
       (let* ((dest (get-destination option cparam))
	      (nnode (get-conversation-node dest cparam)))
	 (ctypecase nnode
	   (keyword (case nnode
		      (:quit (return))
		      (t (warn "Unknown conversation keyword ~A." nnode))))
	   (conversation-node (setf node nnode))))))))

(defun activate-conversation (id player npc)
  (display-conversation id (make-conversation-parameters :player player :npc npc)))

;; macro stuff from here on
(eval-when (:compile-toplevel :load-toplevel)
(defun find-clause (keyword clauses)
  (rest (find keyword clauses :key #'car)))

(defun collect-clauses (keywords clauses)
  (loop for x in clauses
	when (member (car x) keywords) collect x))

(defun conv-closure (pc-sym npc-sym &rest exprs)
  (let ((params (gensym)))
    `(lambda (,params)
      (let ((,pc-sym (cparam.player ,params))
	    (,npc-sym (cparam.npc ,params)))
	(declare (ignorable ,pc-sym ,npc-sym))
	,@exprs))))

(defun convert-text-clause (clause pc-sym npc-sym)
  ;; text clauses are either
  ;; (1) (:text "string")
  ;; (2) (:text "format string" format-args)
  ;; (3) (:text (expression(s) returning string))
  (cond ((and (= (length clause) 1) (stringp (car clause)))
	 (car clause))
	((stringp (car clause))
	 (conv-closure pc-sym npc-sym
		       `(format nil ,(car clause) ,@(cdr clause))))
	(t
	 (apply #'conv-closure pc-sym npc-sym clause))))

(defun convert-id-clause (clause)
  (cond ((null clause)
	 nil)
	((and (= (length clause) 1) (stringp (car clause)))
	 (car clause))
	(t
	 (error "Error in ID clause ~S" clause))))

(defun convert-function-clause (clause pc-sym npc-sym)
  ;; used for both perform and test
  ;; for now, only null or function, maybe add special flag stuff later?
  (cond ((null clause)
	 nil)
	(t
	 (apply #'conv-closure pc-sym npc-sym clause))))

(defun convert-dest-clause (clause)
  ;; only strings and keywords allowed for now
  (cond ((null clause)
	 nil) ;; need to check for either :dest or :node keyword elsewhere
	((and (= (length clause) 1)
	      (or (stringp (car clause))
		  (keywordp (car clause))))
	 (car clause))
	(t
	 (error "Error in DEST clause ~S" clause))))

(defun convert-node-clause (clause pc-sym npc-sym)
  (cond ((null clause)
	 nil)
	(t
	 (convert-node-expression clause pc-sym npc-sym))))

(defun convert-option-clause (clause pc-sym npc-sym)
  ;; test for unknown and required clauses
  (test-option-clauses clause)
  (let ((text (convert-text-clause (find-clause :text clause)
				   pc-sym npc-sym))
	(test (convert-function-clause (find-clause :test clause)
				       pc-sym npc-sym))
	(perform (convert-function-clause (find-clause :perform clause)
					  pc-sym npc-sym))
	(dest (convert-dest-clause (find-clause :dest clause)))
	(node (convert-node-clause (find-clause :node clause)
				   pc-sym npc-sym)))
    ;; assume everything is ok here
    `(%option :text ,text :test ,test :perform ,perform :dest ,(or dest node))))

(defun convert-quit-option-clause (clause)
  `(%quit-option ,@clause))

(defun convert-skip-test (clause pc-sym npc-sym)
  (when clause
    (conv-closure pc-sym npc-sym (first clause))))

(defun convert-skip-dest (clause pc-sym npc-sym)
  (when clause
    (let ((expr (second clause)))
      (ecase (first expr)
	(:dest (assert (and (= (length expr) 2)
			    (stringp (second expr))))
	       (second expr))
	(:node (convert-node-expression (rest expr) pc-sym npc-sym))))))

(defun count-clauses (clauses)
  ;; first collect and count keywords
  (let ((counts (make-hash-table)))
    (dolist (clause clauses)
      (unless (and (consp clause)
		   (keywordp (car clause)))
	(error "DEFINE-CONVERSATION: Each clause must be on the form
 (:KEYWORD &REST STUFF), not ~A" clause))
      (let ((key (car clause)))
	(setf (gethash key counts) (1+ (or (gethash key counts) 0)))))
    counts))

(defun test-conversation-clauses (clauses)
  (let ((counts (count-clauses clauses)))
    ;; check the requirements of each allowed keyword
    (flet ((cnt (key)
	     (or (gethash key counts) 0)))
      (assert (= (cnt :text) 1) ()
	      "Each conversation node requires exactly one :TEXT clause.")
      (assert (>= (+ (cnt :option) (cnt :quit-option)) 1) ()
	      "Each conversation node requires at least one :OPTION or :QUIT-OPTION.")
      (assert (<= (cnt :id) 1) ()
	      "More than one :ID clause in conversation node.")
      (assert (<= (cnt :perform) 1) ()
	      "More than one :PERFORM clause in conversation node.")
      (assert (<= (cnt :skip-if) 1) ()
	      "Too many :SKIP-IF clauses (more than one might be allowed in the future)."))
    ;; make sure there are no unknown clauses
    (loop for key being the hash-keys of counts do
	  (unless (member key '(:text :option :quit-option :id :perform :skip-if))
	    (error "Unknown clause ~S in conversation node." key)))))

(defun test-option-clauses (clauses)
  (let ((counts (count-clauses clauses)))
    ;; check the requirements of each allowed keyword
    (flet ((cnt (key)
	     (or (gethash key counts) 0)))
      (assert (= (cnt :text) 1) ()
	      "Each conversation option requires exactly one :TEXT clause.")
      (assert (= (+ (cnt :dest) (cnt :node)) 1) ()
	      "Each conversation option requires exactly one :DEST or :NODE clause.")
      (assert (<= (cnt :test) 1) ()
	      "More than one :TEST clause in conversation option.")
      (assert (<= (cnt :perform) 1) ()
	      "More than one :PERFORM clause in conversation option."))
    ;; make sure there are no unknown clauses
    (loop for key being the hash-keys of counts do
	  (unless (member key '(:text :dest :node :test :perform))
	    (error "Unknown clause ~S in conversation option." key)))))

(defun convert-node-expression (clause pc-sym npc-sym)
  "Convert conversation expression from macro syntax to functional syntax."
  ;; test for unknown and required clauses
  (test-conversation-clauses clause)
  (let* ((id (convert-id-clause (find-clause :id clause)))
	 (text (convert-text-clause (find-clause :text clause)
				    pc-sym npc-sym))
	 (perform (convert-function-clause (find-clause :perform clause)
					   pc-sym npc-sym))
	 (skip-clause (find-clause :skip-if clause))
	 (skip-test (convert-skip-test skip-clause pc-sym npc-sym))
	 (skip-dest (convert-skip-dest skip-clause pc-sym npc-sym))
	 (options (loop for (keyword . body) in (collect-clauses '(:option :quit-option) clause)
			collect (ecase keyword
				  (:option (convert-option-clause body pc-sym npc-sym))
				  (:quit-option (convert-quit-option-clause body))))))
    `(%conversation :id ,id :text ,text :perform ,perform
      :skip-test ,skip-test :skip-dest ,skip-dest
      :options (list ,@options))))
)

(defmacro define-conversation ((pc-sym npc-sym) &rest args)
  ;; make sure args has an ID clause (the other clauses can be tested in convert-node-expression)
  (unless (find-clause :id args)
    (error "DEFINE-CONVERSATION must have an :ID clause."))
  `(eval-when (:load-toplevel :execute)
    ,(convert-node-expression args pc-sym npc-sym)))

;; some conveniance flagging functions (just shorthands for information table stuff)
(defun set-flag (flag &optional (value t))
  (setf (get-information flag) value))

(defun unset-flag (flag)
  ;; should remove flag?
  (setf (get-information flag) nil))

(defun flag (flag)
  (get-information flag))

;; example
(define-conversation (player npc)
    (:id "alfred")
  ;; first time skip to this node
  (:skip-if
   (not (flag "met-alfred"))
   (:node (:text "Hello, stranger. I don't believe we've met. My name is Alfred.")
	  (:option (:text "Pleased to meet you, I'm ~A." (player.name player))
		   (:perform (set-flag "met-alfred"))
		   (:dest "alfred"))))
  (:text (if (flag "alfred-angry")
	     "I'm not sure I want to talk to you."
	     (format nil "What can I do for you, ~A?" (player.name player))))
  ;; this option is only to make Alfred happy again
  (:option
   (:test (flag "alfred-angry"))
   (:text "I'm sorry if I offended you earlier.")
   (:node (:text "I guess that's okay.")
	  (:perform (unset-flag "alfred-angry"))
	  (:option (:text "Can you help me with something else?")
		   (:dest "alfred"))
	  (:quit-option "See you later.")))
  ;; the rest of the options are always shown
  (:option
   (:text "What can you tell me about this place?")
   (:node (:text "Oh, this is just another crossroad turned into a town. We get dozens of adventurers through here every day, selling loot, drinking mead and looking for work. Haven't you got anything more interesting to ask about?")
	  (:option (:text "I guess I do.")
		   (:dest "alfred"))
	  (:quit-option "Not right now I don't. Bye")))
  (:option
   ;;(:test (flag "heard-of-killer-rabbit"))
   (:text "What can you tell me of a killer rabbit?")
   (:node (:text "My God, you don't actually believe those stories, do you? If you really want more information, try talking to Herman Wilkinson, he can't stop rambling about the bloody 'killer rabbit'. If you ask me, all that mead has done permanent damage to the poor guy. Now PLEASE let ut talk about something else.")
	  (:option (:text "OK then.") (:dest "alfred"))
	  (:option (:text "Can't you tell me what you have heard?")
		   (:node (:text "Aw, go away!")
			  (:perform (set-flag "alfred-angry"))
			  (:quit-option)))
	  (:quit-option "I will go see him right now, bye.")))
  (:option
   (:text "Do you know where I can find work?")
   (:node (:text "The mayor sometimes have work for adventurers.  Other than that I don't know.")
	  (:option (:text "Thank you.") (:dest "alfred"))
	  (:option (:text "Where can I find the mayor?")
		   (:node (:text "Well, the blindingly obvious place would be the town hall, wouldn't it?")
			  (:option (:text "And where is the town hall?")
				   (:node (:text "Do I look like a bloody tourist guide to you? Do I? For god's sake, it's a small town, take a look around!")
					  (:perform (set-flag "alfred-angry"))
					  (:option (:text "Didn't mean to upset you. Can I ask about something else?")
						   (:dest "alfred"))
					  (:quit-option "Um... I think I'll leave you and your temper alone for a while.")))
			  (:option (:text "I suppose.") (:dest "alfred"))))))
  (:quit-option "I have to go now."))

