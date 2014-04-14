(setq EXPV001 'CON004)
(setq CON004 '(SEE AGENT AG002
OBJECT OBJ002))
(setq AG002 'CON005)
(setq CON005 '(HUMAN F-NAME (CHARLOTTE)
L-NAME ( )
GENDER (FEMALE)))
(setq OBJ002 '(HUMAN F-NAME (CHARLES)
L-NAME ( )
GENDER (MALE)))
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

(defun replaceSym (start sym end)
	(cond
		((symbolp sym)
			(cond
			((boundp sym)
				(cond
				((and (null start) (null end)) (eval sym))
				((null start) (append (list (eval sym)) end))
				((null end) (append start (list (eval sym))))
				(t (append start (list (eval sym)) end))
				)
			)
			((and (null start) (null end)) sym)
			((null start) (append (list sym) end))
			((null end) (append start (list sym)))
			(t (append start (list sym) end))
			)
		)
		((and (null start) (null end)) sym)
		((null start) (append (list sym) end))
		((null end) (append start (list sym)))
		(t (append start (list sym) end))
	)
)
(defun UNGAP_helper (frame number)
	(cond
		((not(listp frame)) (replaceSym nil frame nil))
		((null (nth number frame)) frame)
		((not(null(nth number frame))) (UNGAP_helper
			(replaceSym (subseq frame 0 number) (nth number frame) (nthcdr (+ number 1) frame)) (+ number 1)))
		(t frame)
	)
)
;Takes an atom that has a atom or instantiated frame as its value.
;UNGAP generates a new frame with all gaps removed
(defun UNGAP (frame)
	(cond
	((not(equal (UNGAP_helper frame 0) frame)) (UNGAP(UNGAP_helper frame 0)))
	(t frame)
	)
)

;Adds a top-level slot with filler to a frame.
(defun ADD-SF (slot filler frame)

)

(format t "~S"
	(UNGAP 'EXPV001)
)