;remove top level symbol
(defun removeTopLevelSym (frame)
	(cond
		((symbolp frame)
			(cond
				((boundp frame) (removeTopLevelSym (eval frame)))
				(t frame)
			)
		)
		(t frame)
	)
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
	(FILLER_helper slot (removeTopLevelSym frame) 1)
)

;Takes a list of slots and uses them to path into a concept 
;(i.e. an instantiated frame) and returns the frame that is being sought.
(defun PATH-SL (slots concept)
	(cond
		;Base case:
		((null concept) nil)
		((equal (length slots) 1) (FILLER (first slots) (removeTopLevelSym concept))) ;last slot, start to return
		; Recursive case: increment position in frame
		(t (PATH-SL (rest slots) (FILLER (first slots) (removeTopLevelSym concept))))
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

;replace filler
(defun replaceSlot (filler frame number)
	(cond
		((and (not(null (subseq frame 0 number))) (not(null (nthcdr (+ number 1) frame)))) (append (subseq frame 0 number) (list filler) (nthcdr (+ number 1) frame)))
		((not(null (subseq frame 0 number))) (append (subseq frame 0 number) (list filler)))
		((not(null (nthcdr (+ number 1) frame))) (append (list filler) (nthcdr (+ number 1) frame)))
		(t (list filler))
	)
)
;search for slot and replace if found
(defun searchAndReplace (slot filler frame number)
	(cond
		((null(nth number frame)) (append frame (list slot filler)))
		((equal (nth number frame) slot) (replaceSlot filler frame (+ number 1)))
		(t (searchAndReplace slot filler frame (+ number 2)))
	)
)
;Adds a top-level slot with filler to a frame.
(defun ADD-SF (slot filler frame)
	;remove top level filter -> replace filler
	(searchAndReplace slot filler (removeTopLevelSym frame) 1)
)

;is element a member?
(defun isMember (a b)
	(cond
		((equal a b) t)
		((not (listp b)) nil)
		((null a) nil)
		((null b) nil)
		((equal a (first b)) t)
		(t (isMember a (rest b)))
	)
)
;compute subset
(defun subset (a b)
  (if (endp a) T
    (and (isMember (first a) b) (subset (rest a) b)))
)
;find if sets have same members
(defun set-equal (a b)
	(and (subset a b) (subset b a))
)
;return list of top slots
(defun topSlotList (frame)
	(cond
	((null frame) nil)
	((NOT (listp frame)) nil)
	(t (append (List (first frame)) (nthcdr 2 frame)))
	)
)
;Returns T if frame1 and frame2 have the same slot-filler structure.
(defun SAME-SF (frame1 frame2)
	(set-equal (topSlotList (rest (UNGAP frame1))) (topSlotList (rest (UNGAP frame2))))
)