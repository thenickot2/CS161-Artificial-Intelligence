; CS 161 Spring 2014: HW3 skeleton

; Suppresses function redefinition warnings (for
; problems 3 - 5)
(setq CUSTOM:*SUPPRESS-CHECK-REDEFINITION* t)

; Any functions you use from hw 1 - 2 should be imported
; in your test script rather than this file (for grading
; sake rather than good practice!)

; File structure:
;
; Section 1: Utility functions:
;   -- PROBLEM 1: ISA
;   -- PROBLEM 2: INSERT-SL
;   -- PROBLEM 3: SRCH
;   -- PROBLEM 4: TOP-CON
;   -- PROBLEM 5: C-ANALYZER
;
; Section 2: Demons:
;   -- PROBLEM 6: DM-MODIF
;   -- PROBLEM 7: DM-LNAME
;   -- PROBLEM 8: DM-DISAMB-FEELS
;   -- PROBLEM 9: DM-FNAME
;   -- PROBLEM 10: DM-FINDHER
;   -- PROBLEM 11: DM-MODIF2
;   -- PROBLEM 12: DM-HIM-FNAME
;   -- PROBLEM 13: DM-USED

(setq ISAMEM '(
    (ISA  HOME  LOCATION)
    (ISA  HOME  BUILDING)
    (ISA  BUILDING  INANIMATE)
    (ISA  KILL  VIOLENT-ACT)
    (ISA  VIOLENT-ACT  ACT)
    (ISA  READ  VISUAL-ACT)
    (ISA  SEEK  VISUAL-ACT)
    (ISA  VISUAL-ACT  ACT)
    (ISA  EAT  ACT)
    (ISA  CO-HABITATE  ACT)
    (ISA  HUMAN  ANIMATE)
    (ISA  RING  INANIMATE)
    (ISA  PRINTED-MATTER  PHYS-OBJ)
    (ISA  ANIMATE  PHYS-OBJ)
    (ISA  INANIMATE  PHYS-OBJ)
    (ISA  RING  WEAR-OBJ)
    (ISA  WEAR-OJB  PHYS-OBJ)
))

; ****** BEGIN UTILITY FUNCTIONS ******

; -----------------------------------------------------------------------------

; PROBLEM 1: ISA

; PURPOSE: This utility function returns T if atom ATM1 recursively ISA ATM2 
;          when using the ISA memory parameter isam; else ISA returns NIL.
; INPUTS:  atm1: atom to compare
;          atm2: atom to compare
;          isam: ISA ontology (in the format of the global ISAMEM)
; OUTPUTS: T if atm1 ISA atm2 using ISA ontology isam

(defun ISA_helper (atm1 atm2 isam theRest)
    (cond
		((null isam) nil)
		((and (equal atm2 (third isam)) (equal atm1 (second isam))) t)
		((and (equal atm1 (second isam)) (ISA (third isam) atm2 theRest)) t)
		(t (ISA atm1 atm2 theRest))
	)
)
(defun ISA (atm1 atm2 isam)
    (cond
		((null isam) nil)
		((equal atm1 atm2) t)
		(t (ISA_helper atm1 atm2 (first isam) (rest isam)))
	)
)

; -----------------------------------------------------------------------------

; PROBLEM 2: INSERT-SL

; PURPOSE:  Return a FRAME which is a copy of the referenced frame, with the
;           designated SLOT filled in with the given FILLER (or added if the
;           SLOT does not exist)
; INPUTS:   slot: atom (name of slot to fill)
;           filler: FILLER (value to put in SLOT)
;           conatm: bound concept atom (gap (possibly nested) or frame to be modified)
; OUTPUT:   conatm that was modified by the insertion (the atom representing it)

(defun front-slot (frame)
    (second frame)
)
(defun front-filler (frame)
    (third frame)
)
(defun pop-slot (frame)
    (cons (first frame) (nthcdr 3 frame))
)
(defun ADD-SF-EXEC (slot filler frame)
    (cond
        ((<= (length frame) 1) (cons (first frame) (cons slot (list filler))))
        ((equal slot (front-slot frame)) (cons (first frame)
                                               (append
                                                    (append (list slot) (list filler)
                                                    (nthcdr 3 frame))))
        )
        (t (append (ADD-SF-EXEC slot filler (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))
    )
)
(defun ADD-SF (slot filler frame)
    (if (atom frame) 
        (if (boundp frame) (ADD-SF-EXEC slot filler (eval frame)) frame)
        (ADD-SF-EXEC slot filler frame)
    )
)
(defun INSERT-SL (slot filler conatm)
	(progn
		(setq USEDMEM (append USEDMEM (list conatm)))
		(ADD-SF slot filler (eval conatm))
	)
)


; -----------------------------------------------------------------------------

; PROBLEM 3: SRCH

; PURPOSE: SRCH searches through a list of atoms, which is structured as a list
;          of CON atoms that evaluate to frames. It should start its search at 
;          the atom ATMLST that matches MYATM, moving either immediately or 
;          iteratively forward or backward in ATMLST looking for a frame whose
;          top-level predicate ISA pred.
; INPUTS:  atmlst - list of atoms that eval to frames
;          myatm  - atom to start at
;          dir    - direction to search which can be:
;                   AFT -> forward
;                   IM-AFT -> immediately following
;                   BEF -> backward
;                   IM-BEF -> immediately preceding
;          pred   - pred to search for
; OUTPUT:  frame-reference atom if successful, NIL otherwise

(defun START-SRCH (atmlst myatm dir)
    (cond
        ; Why have two cases when we can have one? Just do a forward search in
        ; the reversed list for a BEF search.
        ((equal dir 'BEF) (START-SRCH (reverse atmlst) myatm 'AFT))
        ; We can even condense our "immediately before" case into an 'IM-AFT
        ((equal dir 'IM-BEF) (START-SRCH (reverse atmlst) myatm 'IM-AFT))
        ; Base case: searched everything
        ((null atmlst) nil)
        ; Base case: found what we were looking for, so just return the rest
        ; if we had an AFT dir, or simply the next one in line if we had an
        ; IM-AFT dir
        ((equal (first atmlst) myatm) (if (equal dir 'AFT) (rest atmlst) (list (second atmlst))))
        ; Recursive case: no dice, so keep looking
        (t (START-SRCH (rest atmlst) myatm dir))
    )
)
(defun SRCH (atmlst myatm dir pred &optional found)
    (cond
        ; As a first step, find our starting CON. Flag that we found it
        ; by setting optional parameter "found"
        ((not found) (SRCH (START-SRCH atmlst myatm dir) myatm dir pred t))
        
        ; Base case: searched everything
        ((null atmlst) nil)
        
        ; We already found the start atom, so start looking at predicates
        (t
             ; See if pred matches (have to expand the first atm in atmlst
             ; into its frame representation first)
             (if (ISA (first (eval (first atmlst))) pred ISAMEM)
                 ; A match, so return it
                 (first atmlst)
                 ; Else, no match; remove and try again
                 ; (add optional argument "found" since we did find con)
                 (SRCH (rest atmlst) myatm dir pred t)
             )
        )
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 4: TOP-CON

; PURPOSE: TOP-CON goes through the atoms in WKM and returns a list of all 
;          atoms that do NOT appear in USED AND that are not null.
; INPUTS:  wkm (list)  - working memory atoms
;          used (list) - used atoms
; OUTPUT:  List of atoms in wkm that do not appear in USED AND are not null
(defun TOP-CON-helper (theLst)
	(cond
		((null theLst) nil)
		((null (eval (first theLst))) (TOP-CON-helper (rest theLst)))
		(t (append (list(first theLst)) (TOP-CON-helper (rest theLst))))
	)
)
(defun TOP-CON (wkm used)
    (TOP-CON-helper (set-difference wkm used))
)

; -----------------------------------------------------------------------------

; PROBLEM 5: C-ANALYZER

; PURPOSE: C-ANALYZER is the top-most level function, that will return a frame
;          representation of a sentence represented as a sequence of words. 
;          It loops through attempting to match the words of SENT to words 
;          or phrases in the lexicon, LEXIC, using NEXT-PH. For each 
;          recognized phrase, it INSTAN-CONs the associated frame (also 
;          adding the CON atom of that frame to working memory within 
;          INSTAN-CON), SPAWNs the associated demon list, and invokes 
;          POLL-DEMS on the global list DEMEM. This process repeats until 
;          SENT is empty. C-ANALYZE then returns the UNGAP of the 
;          TOP-CON of working memory as it stands after loading in the 
;          complete sentence.
;          Additionally, as a side effect, it sets USEDMEM = WKMEM.
; INPUTS:  sent (list)  - list of words (as atoms) representing a sentence
;          lexic (list) - a conceptual lexicon (see problem 1)
; OUTPUT:  List of UNGAPPED TOP-CON of WKMEM and USEDMEM
; SIDE-EFFECT: USEDMEM = WKMEM

(defun GAPSLOTS (sf)
    (cond
        ((null sf) nil)
        (t (append (append (list (first sf))            ; rebuild our first slot-filler pair
                           (list (UNGAP (second sf))))  ; dispatch UNGAP on the filler
                           (GAPSLOTS (nthcdr 2 sf))))   ; recurse on rest of sf
    )
)
(defun UNGAP (frame)
    (cond
        ((not (listp frame)) (if (boundp frame) (UNGAP (eval frame)) frame))
        ((<= (length frame) 1) frame)
        (t (cons (first frame) (GAPSLOTS (rest frame))))
    )
)
(defun MATCH-PH (words lex)
    (let* ((len (length lex)))
        (cond
            ; Not enough words left, so no dice
            ((< (length words) len) 0)
            ; First len entries of words indeed match, so return len
            ((equal (subseq words 0 len) lex) len)
            ; Otherwise, return 0
            (t 0)
        )
    )
)
(defun NEXT-PH (wrdlst lexic)
    (cond
        ; Base cases: return nil if either params null
        ((null wrdlst) nil)
        ((null lexic) nil)
        
        ; Got to end of lexic, so return the best match, or special unknown
        ; word frame
        ((= (length lexic) 1)
            (let* (
                (lex (first lexic))
                (match-len (MATCH-PH wrdlst (first lex)))
            )
            (if (> match-len 0)
                ; If match, build list of phrase, frame, demons as the first
                ; element, with the rest of the sentence tacked on after
                (cons (list (subseq wrdlst 0 match-len) (second lex) (third lex))
                      (nthcdr match-len wrdlst))
                
                ; Ff no match, special return value
                (cons (list
                    ; Unknown word goes as its own list in front
                    (list (first wrdlst))
                    ; Build the UNKNOWN frame
                    (list 'UNKNOWN 'WORD (list (first wrdlst)))
                    ; No demons
                    NIL)   
                    ; Remove first word
                    (rest wrdlst))
            ))
        )
        
        ; Recursive case: find best match between first two entries. Keep
        ; first entry if neither match.
        (t (let* (
                (match1 (MATCH-PH wrdlst (first (first lexic))))
                (match2 (MATCH-PH wrdlst (first (second lexic))))
            )
            ; Remove the worst match of the first 2 entries
            (if (>= match1 match2)
                (NEXT-PH wrdlst (cons (first lexic) (nthcdr 2 lexic)))
                (NEXT-PH wrdlst (rest lexic)))
            )
        )
    )
)
(defun SPAWN (partial-dems mycon)
    (cond
        ((null partial-dems) nil)
        (t
            (let* (
                (dem (first partial-dems))
                ; construct new demon as ((first dem) mycon (rest dem))
                (newdemon (append (list (first dem)) (list mycon) (rest dem) )))
                ; Update global DEMEM
                (setq DEMEM (append (list newdemon) DEMEM))
                ; Recurse on any remaining demons
                (cons newdemon (SPAWN (rest partial-dems) mycon))
            )
        )
    )
)(defun POLL-DEMS-EXEC (demlst &optional last-len)
    (cond
        ; Base case: nothing to do, or all demons successful
        ((null demlst) nil)
        ; Length didn't change since last call => quiescence reached
        ((equal (length demlst) last-len) demlst)
        ; gather all unsuccessful demons
        (t (POLL-DEMS-EXEC
            (loop for dem in demlst append
                ; call demon
                (if (apply (first dem) (rest dem))
                    nil ; If success, don't add back
                    (list dem) ; On failure, do add back
                )
            )
            (length demlst) 
            ; recursive call will check whether we
            ; removed anything
            )
        )
    )
)
(defun POLL-DEMS (demlst)
    (setq DEMEM (POLL-DEMS-EXEC demlst))
)
(defun C-ANALYZER (sent lexic)
    ; pfdr = (((phrase) frame (demons*)) rest)
    (let* ((pfdr (NEXT-PH sent lexic))
           ; triplet = ((phrase) frame (demons*))
           (triplet (first pfdr))
           (frame (second triplet))
           (demons (third triplet))
           (nextsent (rest pfdr)))
    
    ; When we've exhausted the sentence, return the UNGAP of TOP-CON
    (if (null sent)
        (loop for fr in (TOP-CON WKMEM USEDMEM) collect (UNGAP fr))
        ; Else, process frame/demons, then recurse
        (progn
            ; Need to spawn demons for the con atom we just added, which
            ; will be the last element of the WKMEM, which we can get using
            ; last, which returns a list, so grab the first element
            (SPAWN demons (first (last (INSTAN-CON frame WKMEM))))
            ; Poll the active demons, which will update the DEMEM after
            ; reaching quiescence
            (POLL-DEMS DEMEM)
            ; Finally, recurse: next part of sentence after removing
            ; matched phrase
			(setq USEDMEM WKMEM)
            (C-ANALYZER nextsent lexic)
        )
    ))
)

; -----------------------------------------------------------------------------

; ****** END UTILITY FUNCTIONS ******



; ****** BEGIN DEMON SECTION ******

; -----------------------------------------------------------------------------

; PROBLEM 6: DM-MODIF

; PURPOSE: Starting at mycon, this demon uses the SRCH function (as modified 
;          above) to look in direction dir for a CON atom with predicate that 
;          ISA pred and if found, uses the utility function INSERT-SL to 
;          insert pair: 
;          [ slot  filler ] into found 's frame and to add found to USEDMEM.
; INPUTS:  mycon (atom) - concept atom this demon works for
;          pred (atom)  - predicate to search for
;          dir (atom)   - direction in which to search
;          slot (atom)  - slot to add to found's frame
;          filler       - FILLER to add at slot in found's frame
; OUTPUTS: (DIE) if search successful, nil otherwise

(defun DM-MODIF (mycon pred dir slot filler)
    (let ((theAtm (SRCH WKMEM mycon dir pred)))
        (cond
			((not(null theAtm)) (progn 
				(set theAtm (INSERT-SL slot filler theAtm))
				'(die)))
			(t nil)
		)
	)
)

; -----------------------------------------------------------------------------

; PROBLEM 7: DM-LNAME

; PURPOSE: This demon uses the SRCH function to look immediately after mycon 
;          for the predicate UNKNOWN with slot-name WORD and if its filler is 
;          found, it sets the gap of myslot to have that filler value. It also 
;          adds the CON atom of UNKNOWN to USEDMEM.
; INPUTS:  mycon (atom)  - concept atom this demon works for
;          myslot (atom) - slot name in mycon frame to set to found slot WORD
; OUTPUTS: (DIE) if search successful, nil otherwise
; SIDE-EFFECT: Adds the con atom of UNKNOWN to USEDMEM

(defun FILLER (slot frame)
    (cond
        ((<= (length frame) 1) nil)
        ((equal slot (front-slot frame)) (front-filler frame))
        (t (FILLER slot (pop-slot frame)))
    )
)
(defun DM-LNAME (mycon myslot)
    (let ((theAtm (SRCH WKMEM mycon 'IM-AFT 'UNKNOWN)))
		(cond
			((and(not(null theAtm)) (not(null(FILLER 'WORD (eval theAtm))))) (progn
				(set (FILLER myslot (eval mycon)) (FILLER 'WORD (eval theAtm)))
				(setq USEDMEM (append USEDMEM (list theAtm)))
				'(die)
			)
			)
			(t nil)
		)
	)
)

; -----------------------------------------------------------------------------

; PROBLEM 8: DM-DISAMB-FEELS

; PURPOSE: This demon attempts to disambiguate the input word. This demon uses 
;          SRCH to look immediately after MYCON for the predicate THAT and if 
;          found uses UNIQUE-GAPS (from HW2) to instantiate a frame:
;          (BELIEVE AGENT AG OBJECT OBJ) as the value of mycon and then uses 
;          the function SPAWN (from HW2) to spawn two DM-EXP demons 
;          (one for AGENT, the other for OBJECT) else it selects: 
;          (STATE TYPE TYP AGENT AG) as the frame for the word FEELS and 
;          spawns two DM-EXP demons (one for TYPE and one for AGENT).
; INPUTS:  mycon (atom) - concept atom this demon works for
; OUTPUTS: (DIE) if search successful, nil otherwise

(setq *gensym-counter* 1)
(defun NEWATM (symName)
    ; Generate the new symbol using gensym
    (let* ((new-sym (gensym (string symName))))
        (intern (string new-sym))
    )
)
(defun UNIQUE-GAPSLOTS (sf)
    (cond
        ; Base case: got through them all
        ((null sf) nil)
        ; Recursive case: dispatch TOKENIZE on our first filler
        (t (append (append (list (first sf))                  ; rebuild our first slot-filler pair
                           (list (UNIQUE-GAPS (second sf))))  ; dispatch UNIQUE-GAPS on the filler
                           (UNIQUE-GAPSLOTS (nthcdr 2 sf))))  ; recurse on rest of sf
    )
)
(defun UNIQUE-GAPS (frame)
    (cond
        ; Base case: we got an atom, so uniquify it, and return it
        ((not (listp frame)) (let* ((gap (NEWATM frame))) gap))
        ; Base case: empty, or single pred frame
        ((<= (length frame) 1) frame)
        ; Main case: dispatch UNIQUE-GAPSLOTS on our slot-filler list
        (t (cons (first frame) (UNIQUE-GAPSLOTS (rest frame))))
    )
)
(defun DM-DISAMB-FEELS (mycon)
    (let ((theAtm (SRCH WKMEM mycon 'IM-AFT 'THAT)))
		(progn
		(cond
			((null theAtm) (progn 
				(set mycon (UNIQUE-GAPS '(STATE TYPE TYP AGENT AG)))
				(SPAWN '((DM-EXP ACT AFT EMOTION)(DM-EXP HUMAN BEF AGENT)) (first(list mycon)))
			))
			(t (progn 
				(set mycon (UNIQUE-GAPS '(BELIEVE AGENT AG OBJECT OBJ)))
				(SPAWN '((DM-EXP ACT AFT OBJECT)(DM-EXP HUMAN BEF AGENT)) (first(list mycon)))
			))
		)
		'(die))
	)
)

; -----------------------------------------------------------------------------

; PROBLEM 9: DM-FNAME

; PURPOSE: Looks for pred in direction dir with a slot-name myslot and gets 
;          its filler (this is found). If found, it binds the gap of myslot 
;          (within the frame represented by mycon) with found and adds the CON 
;          atom of the frame it found to USEDMEM.
; INPUTS:  mycon (atom)  - concept atom this demon works for
;          pred (atom)   - predicate to search for
;          dir (atom)    - direction in which to search
;          myslot (atom) - slot name containing gap within mycon frame to bind
; OUTPUTS: (DIE) if search successful, nil otherwise
; SIDE-EFFECT: Adds found CON atom to USEDMEM

(defun DM-FNAME (mycon pred dir myslot)
    (let ((theAtm (SRCH WKMEM mycon dir pred)))
		(cond
			((not(null theAtm)) (progn
				(set (FILLER myslot (eval mycon)) (FILLER myslot (eval theAtm)))
				(setq USEDMEM (append USEDMEM (list theAtm)))
				'(die)
			))
			(t nil)
		)
	)
)

; -----------------------------------------------------------------------------

; PROBLEM 10: DM-FINDHER

; PURPOSE: Looks for a CON atom in direction dir whose frame has top-level 
;          predicate pred with slot slot and filler filler. If found, 
;          DM-FINDHER binds found to the gap of myslot. 
; INPUTS:  mycon (atom)  - concept atom this demon works for
;          pred (atom)   - predicate to search for
;          slot (atom)   - slot within query frame
;          filler        - FILLER to match within slot of query frame
;          dir (atom)    - direction in which to search
;          myslot (atom) - slot name containing gap within mycon frame to bind
; OUTPUTS: (DIE) if search successful, nil otherwise
; SIDE-EFFECT: Adds found CON atom to USEDMEM

(defun DM-FINDHER (mycon pred slot filler dir myslot)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------

; PROBLEM 11: DM-MODIF2

; PURPOSE: Starting at mycon, this demon looks in direction dir for a CON atom 
;          with a pred that isa mypred and if found, uses INSERT-SL to insert 
;          myslot with the filler being the gap of myslot into the top level 
;          of the frame associated with found.
; INPUTS:  mycon (atom)  - concept atom this demon works for
;          mypred (atom) - predicate to search for
;          dir (atom)    - direction in which to search
;          myslot (atom) - slot to add/change in query frame to insert gap to
; OUTPUTS: (DIE) if search successful, nil otherwise

(defun DM-MODIF2 (mycon mypred dir myslot)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------

; PROBLEM 12: DM-HIM-FNAME

; PURPOSE: This demon looks for a CON atom in direction dir that has a 
;          top-level pred with slot slot having filler value filler. If found, 
;          DM-HIM-FNAME looks to see if found has a slot myslot. If so, it sets 
;          the value of myslot in the found frame to be the value of myslot in 
;          the frame of that CON atom.
; INPUTS:  mycon (atom)  - concept atom this demon works for
;          myslot (atom) - slot belonging to mycon frame
;          dir (atom)    - direction in which to search
;          pred (atom)   - predicate to search for
;          slot (atom)   - slot belonging to query frame
;          filler        - FILLER belonging to slot of query frame
; OUTPUTS: (DIE) if search successful, nil otherwise

(defun DM-HIM-FNAME (mycon myslot dir pred slot filler)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------

; PROBLEM 13: DM-USED

; PURPOSE: This demon simply adds MYCON to USEDMEM.
; INPUTS:  mycon (atom) - concept atom this demon works for
; OUTPUTS: (DIE) if search successful, nil otherwise

(defun DM-USED (mycon)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------

; ****** END DEMON SECTION ******
