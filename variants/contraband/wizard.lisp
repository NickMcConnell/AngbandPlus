(in-package :org.langband.contraband)


(define-key-operation 'break-game
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(break)))


(define-key-operation 'wizard-menu
    #'(lambda (dungeon player)

	;;	(with-new-screen ()
	(block wizard-input 
	  (let ((loc-table (gethash :wizard *current-key-table*)))
	    (loop
	     ;;	(clear-window *cur-win*)
	     ;;	(display-creature *variant* player)
	     (print-message! nil)
	     (with-frame (+query-frame+)
	       (put-coloured-line! +term-white+ "Wizard command: " 0 0)

	       (let* ((ch (read-one-character))
		      (fun (check-keypress loc-table ch)))
		 (cond ((and fun (functionp fun))
			(return-from wizard-input (funcall fun dungeon player)))
		       ((eql ch +escape+)
			(return-from wizard-input t))
		       (t
			;; nil
			)))
	       ))))
	))

;; just for testing
(define-key-operation 'dummy-conversation
    #'interactive-start-conversation)


;; Ctrl-A
(define-keypress *ang-keys* :global (code-char 1) 'wizard-menu)

;; wizard stuff

(define-keypress *ang-keys* :wizard #\B 'break-game)
(define-keypress *ang-keys* :wizard #\t 'dummy-conversation)
