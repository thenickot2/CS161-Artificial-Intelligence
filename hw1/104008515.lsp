(setq CON-SENT2 '(CO-HABITATE AGENT (HUMAN F-NAME (CHARLES)
											L-NAME (OAKLEY)
											GENDER (MALE)
											HISTORY (UNKNOWN))
							FAMREL (UNCLE-OF OBJECT (HUMAN F-NAME (CHARLOTTE)
															L-NAME (NEWTON)
															GENDER (FEMALE)))
							OBJECT (HUMAN F-NAME (EMMA)
											L-NAME ( )
											GENDER (FEMALE))
							FAMREL (MOTHER-OF OBJECT (HUMAN F-NAME (CHARLOTTE)
															L-NAME (NEWTON)
															GENDER (FEMALE)))
							TIME (FUTURE))
)

;Recursively finds slot and return filler(position+1).
(defun FILLER_helper (slot frame number)
    (cond
        ; Base case:
		((null (nth number frame)) nil)
        ((equal slot (nth number frame)) (nth (+ number 1) frame))
        ; Recursive case: increment position in frame by 2
        (t (FILLER_helper slot frame (+ number 2)))
    )
)
;Takes a slot and a frame and returns the filler of that slot.
(defun FILLER (slot frame)
	(FILLER_helper slot frame 1)
)

;Takes a list of slots and uses them to path into a concept 
;(i.e. an instantiated frame) and returns the frame that is being sought.
(defun PATH-SL (slots concept)
	(cond
	;Base case:
	((null concept) nil)
	((equal (length slots) 1) (FILLER (first slots) concept)) ;last slot, start to return
	; Recursive case: increment position in frame
	(t (PATH-SL (rest slots) (FILLER (first slots) concept)))
	)
)

;Takes an atom that has a atom or instantiated frame as its value.
;UNGAP generates a new frame with all gaps removed
(defun UNGAP (atom)
; Your code here
)

(format t "~S"
	(PATH-SL '(FAMREL OBJECT) CON-SENT2)
)