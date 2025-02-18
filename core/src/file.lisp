;;; file.lisp --- Parsing and decoding

;; Copyright (C) 2018, 2019, 2024, 2025 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of TFM.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:

;; #### NOTE: it is a bit surprising, but according to the ofm2opl sources,
;; many 8bits-based constants from the original TFM code are still in use for
;; level 0 OFM fonts, even though some data structures have a double size.


;;; Code:

(in-package :net.didierverna.tfm)
(in-readtable :net.didierverna.tfm)


;; ==========================================================================
;; Header
;; ==========================================================================

(define-condition invalid-design-size (tfm-compliance-error)
  ((reference :initform 10) ; slot merge
   (value
    :documentation "The invalid design size."
    :initarg :value
    :reader value))
  (:documentation "The Invalid Design Size compliance error.
It signals that a design size is too small (< 1pt)."))

(define-condition-report (condition invalid-design-size)
    "~Apt design size is too small (< 1pt)"
  (value condition))


(define-condition invalid-original-design-size (tfm-compliance-warning)
  ((reference :initform 10) ; slot merge
   (value
    :documentation "The invalid original design size."
    :initarg :value
    :reader value))
  (:documentation "The Invalid Original Design Size compliance warning.
It signals that, although overridden explicitly, an original design size was
too small (< 1pt)."))

(define-condition-report (condition invalid-original-design-size)
    "~Apt original design size was too small (< 1pt)"
  (value condition))


(defclass padded-string-context (context)
  ((name :documentation "The string name." :initarg :name :reader name))
  (:documentation "The Padded String Context class."))

(defmethod context-string ((context padded-string-context))
  "Return padded string CONTEXT'string."
  (format nil "while reading the ~A string"
    (name context)))


(defun parse-header (length font)
  "Parse a header of LENGTH words from *STREAM* into FONT.
If FONT's design size is less than 1pt, signal an INVALID-DESIGN-SIZE error.
This error is immediately restartable with SET-TO-TEN.
However, if FONT's design size was explicitly overridden, only signal an
INVALID-ORIGINAL-DESIGN-SIZE warning."
  ;; #### NOTE: LENGTH >= 2 has already been checked by the caller
  ;; (LOAD-STREAM).
  (setf (slot-value font 'checksum) (read-u32 nil))
  (decf length)
  (setf (slot-value font 'original-design-size) (read-fix-word nil))
  (unless (>= (original-design-size font) 1)
    (if (design-size font)
      (warn 'invalid-original-design-size :value (original-design-size font))
      (restart-case
	  (error 'invalid-design-size :value (original-design-size font))
	(set-to-ten () :report "Set it to 10pt."
	  (setf (slot-value font 'original-design-size) 10)))))
  (unless (design-size font)
    (setf (design-size font) (original-design-size font)))
  (decf length)
  ;; #### NOTE: we silently assume Xerox PARC headers below. Not sure if
  ;; anything else could be in use, but it's impossible to tell from the files
  ;; themselves. TeX Live 2019 doesn't seem to have anything else.
  #i(checking-length 1)
  (macrolet ((checking-length (needed &body body)
	       "Execute BODY if LENGTH >= NEEDED.
If so, decrease LENGTH by NEEDED afterwards."
	       `(when (>= length ,needed) ,@body (decf length ,needed))))
    (with-condition-context
	(padded-string padded-string-context :name "character encoding scheme")
      (checking-length 10
	(setf (slot-value font 'encoding) (read-padded-string 40))))
    ;; #### FIXME: is it possible for OFM data to be math symbol or extension?
    ;; If so, the code below is wrong because those classes are only
    ;; subclasses of regular TFM.
    (when (encoding font)
      ;; #### NOTE: we don't upcase the BCPL strings, but tftopl does, so it's
      ;; probably better to do string comparisons on upcased versions. Also,
      ;; tftopl only checks for SY and EX, not the full strings, but I don't
      ;; think that really matters.
      (cond ((string= (string-upcase (encoding font)) "TEX MATH SYMBOLS")
	     (change-class font 'math-symbols-font))
	    ((string= (string-upcase (encoding font)) "TEX MATH EXTENSION")
	     (change-class font 'math-extension-font))))
    (with-condition-context
	(padded-string padded-string-context :name "font identifier")
      (checking-length 5
	(setf (slot-value font 'family) (read-padded-string 20))))
    (checking-length 1
      (let ((word (read-u32 nil)))
	(setf (slot-value font '7bits-safe) (ldb (byte 1 31) word))
	(let ((face (ldb (byte 8 0) word)))
	  (setf (slot-value font 'face-number) face)
	  (when (< face 18)
	    (setf (slot-value font 'face-code) (make-string 3))
	    (case face
	      ((0 1  6  7 12 13)
	       (setf (slot-value font 'weight) :medium
		     (aref (face-code font) 0) #\M))
	      ((2 3  8  9 14 15)
	       (setf (slot-value font 'weight) :bold
		     (aref (face-code font) 0) #\B))
	      ((4 5 10 11 16 17)
	       (setf (slot-value font 'weight) :light
		     (aref (face-code font) 0) #\L)))
	    (case face
	      ((0 2 4 6 8 10 12 14 16)
	       (setf (slot-value font 'slope)  :roman
		     (aref (face-code font) 1) #\R))
	      ((1 3 5 7 9 11 13 15 17)
	       (setf (slot-value font 'slope)  :bold
		     (aref (face-code font) 1) #\I)))
	    (case face
	      ((0  1  2  3  4  5)
	       (setf (slot-value font 'expansion) :regular
		     (aref (face-code font) 2)    #\R))
	      ((6  7  8  9  10 11)
	       (setf (slot-value font 'expansion) :condensed
		     (aref (face-code font) 2)    #\C))
	      ((12 13 14 15 16 17)
	       (setf (slot-value font 'expansion) :extended
		     (aref (face-code font) 2)    #\E))))))))
  ;; #### TODO: if applicable, maybe store the remainder somewhere instead of
  ;; just discarding it.
  (loop :repeat length :do (read-u32 nil)))




;; ==========================================================================
;; Character Information Tables
;; ==========================================================================

(define-condition tfm-table-error (tfm-compliance-error)
  ((name :documentation "The table's name." :initarg :name :reader name))
  (:documentation "The TFM table errors root condition.
This is the root condition for errors related to TFM tables."))

(define-condition invalid-table-index (tfm-table-error)
  ((reference :initform 8) ; slot merge
   (index
    :documentation "The invalid index."
    :initarg :value
    :reader value)
   (largest
    :documentation "The largest index."
    :initarg :largest
    :reader largest))
  (:documentation "The Invalid Table Index compliance error.
It signals that a table index is greater than its largest value."))

(define-condition-report (condition invalid-table-index)
  "index ~A in ~A table is invalid (largest is ~A)"
  (index condition)
  (name condition)
  (largest condition))


(defun table-aref (name table index)
  "Access NAMEd TABLE at INDEX.
If INDEX is out of bounds, signal an INVALID-TABLE-INDEX error."
  (unless (< index (length table))
    (error 'invalid-table-index
      :index index :largest (1- (length table)) :name name))
  (aref table index))

(defmacro tref (table index)
  "Call TABLE-AREF, computing the table name from TABLE."
  `(table-aref ,(string-downcase (symbol-name table)) ,table ,index))


;; -------------------------
;; Ligature/Kerning Programs
;; -------------------------

(define-condition invalid-ligature-opcode (tfm-compliance-error)
  ((reference :initform 13) ; slot merge
   (opcode
    :documentation "The invalid ligature opcode."
    :initarg :opcode
    :reader opcode))
  (:documentation "The Invalid Ligature Opcode compliance error.
It signals that a ligature opcode is invalid."))

(define-condition-report (condition invalid-ligature-opcode)
  "ligature opcode ~A is invalid"
  (opcode condition))


(defun %run-ligature/kerning-program
    (character index lig/kerns kerns &aux (font (font character)))
  "Run a ligature/kerning program for CHARACTER.
The program starts at LIG/KERNS[INDEX] and uses the KERNS array. Running the
program eventually creates ligatures or kerns for CHARACTER and some other
character.

If an invalid index into LIG/KERNS is encountered, signal an
INVALID-TABLE-INDEX error. This error is immediately restartable with
ABORT-LIG/KERN-PROGRAM.

If an invalid ligature opcode is encountered, signal an
INVALID-LIGATURE-OPCODE error. This error is immediately restartable with
DISCARD-LIGATURE.

If an invalid index into KERNS is encountered, signal an INVALID-TABLE-INDEX
error. This error is immediately restartable with DISCARD-KERN.

Finally, if an invalid character code is encountered, signal an
INVALID-CHARACTER-CODE error. Depending on the context, this error is
immediately restartable with DISCARD-LIGATURE or DISCARD-KERN."
  (loop
    :for lig/kern
      := (with-simple-restart
	     (abort-lig/kern-program "Abort the ligature/kerning program.")
	   (tref lig/kerns index))
    :while lig/kern
    :unless (> (skip lig/kern) 128)
      :do (if (< (op lig/kern) 128)
	    ;; ligature instruction
	    (let ((opcode (op lig/kern)))
	      (with-simple-restart
		  (discard-ligature "Discard this ligature instruction.")
		(if (or (= opcode 4) (and (> opcode 7) (not (= opcode 11))))
		  (error 'invalid-ligature-opcode :opcode opcode)
		  (set-ligature character (code-character (next lig/kern) font)
				(make-ligature
				 (code-character (rmd lig/kern) font)
				 (when (member opcode '(0 1 5)) t)
				 (when (member opcode '(0 2 6)) t)
				 (cond ((member opcode '(0 1 2 3)) 0)
				       ((member opcode '(5 6 7)) 1)
				       ((= opcode 11) 2)))))))
	    ;; kerning instruction
	    (with-simple-restart
		(discard-kern "Discard this kerning instruction.")
	      (set-kern character (code-character (next lig/kern) font)
			(tref kerns (+ (* 256 (- (op lig/kern) 128))
				       (rmd lig/kern))))))
    :if (>= (skip lig/kern) 128)
      :return t
    ;; #### NOTE: because of the way the next instruction is computed below,
    ;; it is inherently impossible to have a cycle in a lig/kern program.
    :else
      :do (incf index (1+ (skip lig/kern)))))

(defun run-ligature/kerning-program
    (character index lig/kerns kerns
     &aux (lig/kern
	   (with-simple-restart
	       (abort-lig/kern-program "Abort the ligature/kerning program.")
	     (tref lig/kerns index))))
  "Find the real start of a ligature/kerning program and run it.
See %run-ligature/kerning-program for more information.

If INDEX is invalid, signal an INVALID-TABLE-INDEX error. This error is
immediately restartable with ABORT-LIG/KERN-PROGRAM."
  (when lig/kern
    (%run-ligature/kerning-program
     character
     (if (> (skip lig/kern) 128)
       (+ (* 256 (op lig/kern)) (rmd lig/kern))
       index)
     lig/kerns
     kerns)))


;; -----------------
;; Extension Recipes
;; -----------------

;; #### NOTE: the only caller of this function already wraps it into a
;; restart, so we don't need to add one here.
(defun font-extension-recipe (exten font &aux initargs)
  "Make an extension recipe based on EXTEN with FONT's characters."
  (loop :for initarg :in '(:top-character :middle-character :bottom-character)
	:for code :in (list (top exten) (mid exten) (bot exten))
	:unless (zerop code)
	  :do (push (code-character code font) initargs)
	  :and :do (push initarg initargs))
  (apply #'make-extension-recipe (code-character (rep exten) font)
	 initargs))


;; ---------------------
;; Character Information
;; ---------------------

(define-condition invalid-table-start (tfm-table-error)
  ((reference :initform 11) ; slot merge
   (value
    :documentation "The invalid first table value."
    :initarg :value
    :reader value))
  (:documentation "The Invalid Table Start compliance error.
It signals that the first value in a TFM table is not 0."))

(define-condition-report (condition invalid-table-start)
    "first value ~A in ~A table is invalid (should be 0)"
  (value condition)
  (name condition))


(define-condition no-boundary-character (tfm-compliance-error)
  ((reference :initform 13)) ; slot merge
  (:documentation "The No Boundary Character compliance error.
It signals that a boundary character ligature/kerning program was found,
without a boundary character being defined."))

(define-condition-report (condition no-boundary-character)
    "found a boundary character ligature/kerning program,~
without a boundary character being defined")


(define-condition character-list-cycle (tfm-compliance-error)
  ((reference :initform 12) ; slot merge
   (character-list
    :documentation "The cyclic character list."
    :initarg :character-list
    :reader character-list))
  (:documentation "The Character List Cycle compliance error.
It signals that a cycle was found in a list of ascending character sizes."))

(define-condition-report (condition character-list-cycle)
    "found a cycle in character list ~A"
  (character-list condition))


(define-condition ligature-cycle (tfm-compliance-error)
  ((reference :initform 13) ; slot merge
   (ligature
    :documentation "The ligature introducing a cycle."
    :initarg :ligature
    :reader ligature)
   (characters
    :documentation "The cons of characters involved in the ligature."
    :initarg :characters
    :reader characters))
  (:documentation "The Ligature Cycle compliance error.
It signals that a ligature introduces a cycle for a cons of characters."))

(define-condition-report (condition ligature-cycle)
    "ligature ~A introduces a cycle for characters ~A"
  (ligature condition)
  (characters condition))


(defclass table-context (context)
  ((name
    :documentation "The table name."
    :initarg :name
    :reader name)
   (index
    :documentation "The index in the table."
    :initarg :index
    :reader index)
   (size
    :documentation "The table size."
    :initarg :size
    :reader size))
  (:documentation "The Table Context class."))

(defmethod context-string ((context table-context))
  "Return table CONTEXT string."
  (format nil "while parsing ~A table at position ~D/~D"
    (name context)
    (index context)
    (1- (size context))))

(defclass char-info-table-context (table-context)
  ((code
    :documentation "The corresponding character code."
    :initarg :code
    :reader code))
  (:documentation "The Char Info Table Context class."))

(defmethod context-string ((context char-info-table-context))
  "Return char info table CONTEXT string."
  (format nil "~A, for character code ~A" (call-next-method) (code context)))


(defun parse-character-information (nc nw nh nd ni nl nk ne font)
  "Parse the 8 character information tables from *STREAM* into FONT.
NC (EC - BC + 1), NW, NH, ND, NI, NL, NK, and NE are the declared numbers of
entries in the 8 tables, that is, the char infos, widths, heights, depths,
italic corrections, lig/kern instructions, kerns, and extens respectively.

If a char info structure with a width index of 0 is not completely zero'ed
out, signal an SPURIOUS-CHAR-INFO warning.

If the first entry in the widths, heights, depths, or italic corrections table
is not 0, signal an INVALID-TABLE-START error. This error is immediately
restartable with SET-TO-ZERO.

If an index into the widths, heights, depths, or italic corrections tables is
invalid, signal an INVALID-TABLE-INDEX error. This error is immediately
restartable with SET-TO-ZERO.

If a lig/kern program is found for a boundary character, but there is no such
character in the font, signal a NO-BOUNDARY-CHARACTER error. This error is
immediately restartable with ABORT-LIG/KERN-PROGRAM.

If an index into the extens table is invalid, signal an INVALID-TABLE-INDEX
error. This error is immediately restartable with DISCARD-EXTENSION-RECIPE.

If a cycle is found in a list of characters of ascending size, signal a
CHARACTER-LIST-CYCLE error. This error is immediately restartable with
DISCARD-NEXT-CHARACTER.

If a ligature is found to be cyclic, signal a LIGATURE-CYCLE error. This error
is immediately restartable with DISCARD-LIGATURE.

Finally, if an invalid character code is encountered, signal an
INVALID-CHARACTER-CODE error. Depending on the context, this error is
immediately restartable with DISCARD-NEXT-CHARACTER, or
DISCARD-EXTENSION-RECIPE."
  (let ((char-infos (make-array nc :fill-pointer 0))
	(widths (make-array nw :fill-pointer 0))
	(heights (make-array nh :fill-pointer 0))
	(depths (make-array nd :fill-pointer 0))
	(italics (make-array ni :fill-pointer 0))
	(lig/kerns (make-array nl :fill-pointer 0))
	(kerns (make-array nk :fill-pointer 0))
	(extens (make-array ne :fill-pointer 0)))

    ;; 1. Read the tables.
    ;; #### NOTE: the test for (ZEROP NC) below is technically not necessary,
    ;; but the font's MIN-CODE is NIL when there is no character, so the
    ;; second LOOP clause would evaluate to ":from NIL", which could still
    ;; signal a warning even if there is no iteration.
    (unless (zerop nc)
      (loop :with char-info-reader
	      := (typecase font
		   (ofm0-font #'read-ofm0-char-info)
		   (t #'read-char-info))
	    :for i :from 0 :upto (1- nc)
	    :for code :from (min-code font)
	    :do (with-condition-context
		    (spurious-char-info char-info-table-context
		      :name "char info" :code code :index i :size nc)
		  (vector-push (funcall char-info-reader) char-infos))))
    (loop :for name :in (list "widths" "heights" "depths" "italic corrections")
	  :for array :in (list widths heights depths italics)
	  :for length :in (list nw nh nd ni)
	  :do (let ((start (read-fix-word)))
		(unless (zerop start)
		  (restart-case
		      (error 'invalid-table-start :value start :name name)
		    (set-to-zero () :report "Set to 0."
		      (setq start 0))))
		(vector-push start array))
	  :do (loop :for i :from 1 :upto (1- length)
		    :do (with-condition-context
			    (fix-word-overflow table-context
			      :name name :index i :size length)
			  (vector-push (read-fix-word) array))))
    (loop :with lig/kern-reader
	    := (typecase font
		 (ofm0-font #'read-ofm0-lig/kern)
		 (t #'read-lig/kern))
	  :repeat nl
	  :do (vector-push (funcall lig/kern-reader) lig/kerns))
    (loop :repeat nk
	  :do (vector-push (read-fix-word) kerns))
    (loop :with exten-reader
	    := (typecase font
		 (ofm0-font #'read-ofm0-exten)
		 (t #'read-exten))
	  :repeat ne
	  :do (vector-push (funcall exten-reader) extens))

    ;; 2. Create the character metrics.
    ;; #### NOTE: see comment above the first LOOP call about this test.
    (unless (zerop nc)
      (loop :for char-info :across char-infos
	    :for code :from (min-code font)
	    :unless (zerop (width-index char-info))
	      :do (setf (code-character font)
			(make-character-metrics
			 code
			 font
			 (restart-case (tref widths (width-index char-info))
			   (set-to-zero () :report "Use a width of 0."
			     0))
			 (restart-case (tref heights (height-index char-info))
			   (set-to-zero () :report "Use an height of 0."
			     0))
			 (restart-case (tref depths (depth-index char-info))
			   (set-to-zero () :report "Use a depth of 0."
			     0))
			 (restart-case (tref italics (italic-index char-info))
			   (set-to-zero ()
			     :report "Use an italic correction of 0."
			     0))))))
    ;; #### NOTE: this count doesn't (and shouldn't) include a zero'ed out
    ;; boundary character potentially added below.
    (setf (slot-value font 'character-count)
	  (hash-table-count (characters font)))

    ;; Now that we have all the characters registered, we can start processing
    ;; mutual references.

    ;; 3. Check for a boundary character and an associated lig/kern program.
    ;; #### NOTE: boundary characters is an obscure matter, even for
    ;; old-timers. See this thread for some (mis)information:
    ;; https://tug.org/pipermail/texhax/2019-September/023988.html
    (unless (zerop nl)
      (let ((lig/kern (aref lig/kerns 0)))
	(when (= (skip lig/kern) 255)
	  (let ((code (next lig/kern)))
	    (setf (slot-value font 'boundary-character)
		  (or (code-character code font nil)
		      (setf (code-character font)
			    (make-character-metrics code font 0 0 0 0)))))))
      (let ((lig/kern (aref lig/kerns (1- nl))))
	(when (= (skip lig/kern) 255)
	  ;; #### NOTE: since we need to access the last instruction in the
	  ;; lig/kern table, we may as well bypass
	  ;; RUN-LIGATURE/KERNING-PROGRAM.
	  (if (boundary-character font)
	    (%run-ligature/kerning-program
	     (boundary-character font)
	     (+ (* 256 (op lig/kern)) (rmd lig/kern))
	     lig/kerns
	     kerns)
	    (with-simple-restart
		(abort-lig/kern-program "Abort the ligature/kerning program.")
	      (error 'no-boundary-character))))))

    ;; 4. Process ligature / kerning programs, character lists, and extension
    ;; recipes, character by character.
    ;; #### NOTE: see comment above the first LOOP call about this test.
    (unless (zerop nc)
      (loop :for char-info :across char-infos
	    :for code :from (min-code font)
	    ;; #### NOTE: technically, this check is not needed because Step 2
	    ;; made sure that such an (inexistent) character is completely
	    ;; zero'ed out. But it's cleaner and more explicit to keep it,
	    ;; plus it avoids the 3 useless COND checks below.
	    :unless (zerop (width-index char-info))
	      :do (cond ((lig/kern-index char-info)
			 ;; No need to protect anything here. We know
			 ;; accessing the character of CODE is ok because of
			 ;; Step 2.
			 (run-ligature/kerning-program
			  (code-character code font)
			  (lig/kern-index char-info)
			  lig/kerns
			  kerns))
			((next-char char-info)
			 ;; We're not sure about the next character below,
			 ;; however.
			 (with-simple-restart
			     (discard-next-character
			      "Discard the next character.")
			   (setf (slot-value (code-character code font)
					     'next-character)
				 (code-character (next-char char-info) font))))
			((exten-index char-info)
			 ;; And neither about those in the extension recipe
			 ;; below.
			 (with-simple-restart
			     (discard-extension-recipe
			      "Discard this extension recipe.")
			   (setf (slot-value (code-character code font)
					     'extension-recipe)
				 (font-extension-recipe
				  (tref extens (exten-index char-info))
				  font))))))))

  ;; We're done with the tables now.

  ;; #### WARNING: the two checks below have not been tested thoroughly. They
  ;; have been applied to all fonts in TeX Live, but not on fonts made
  ;; explicitly to contain cycles, so we're not really sure that they work.

  ;; 5. Check for cycles in character lists, character by character. Note that
  ;; this is not the best way to do it, as we will end up checking the same
  ;; lists multiple times from different entry points, but who cares (at least
  ;; for now).
  (maphash (lambda (code character)
	     (declare (ignore code))
	     (when (next-character character)
	       (loop :with seen := (list character)
		     :while (next-character character)
		     :if (member (next-character character) seen)
		       :do (restart-case
			       (error 'character-list-cycle
				 :character-list seen)
			     (discard-next-character ()
			       :report "Discard the cyclic next character."
			       (setf (slot-value character 'next-character)
				     nil)))
		     :else
		       :do (push (next-character character) seen)
		       :and :do (setq character (next-character character)))))
	   (characters font))

  ;; 6. Check for ligature cycles, ligature by ligature. Again, this is
  ;; perhaps not the most efficient way to do it and maybe I should study the
  ;; algorithm used in TFtoPL[88..], but we don't care right now.
  (maphash
   (lambda (characters first-ligature)
     (loop :with state := (list (car characters) (cdr characters))
	   :with seen := (list state)
	   :with ligature := first-ligature
	   :while ligature
	   :do (setq state (apply-ligature ligature state))
	   :do (cond ((= (length state) 1)
		      (setq ligature nil))
		     ((member-if (lambda (elt)
				   (and (eq (car state) (car elt))
					(eq (cadr state) (cadr elt))))
				 seen)
		      (restart-case
			  (error 'ligature-cycle
			    :ligature first-ligature
			    :characters characters)
			(discard-ligature ()
			  :report "Discard the ligature."
			  (remhash characters (ligatures font))
			  (setq ligature nil))))
		     (t
		      (push state seen)
		      (setq ligature
			    (get-ligature (car state) (cadr state)))))))
   (ligatures font)))




;; ==========================================================================
;; Parameters Section
;; ==========================================================================

;; #### WARNING: this symbol-macro intentionally captures LENGTH and SLOT!
(symbol-macrolet ((read-parameter
		    (when (>= length 1)
		      (setf slot (read-fix-word))
		      (decf length))))
  (defgeneric parse-parameters (length font)
    (:documentation
     "Parse a parameters section of LENGTH words from *STREAM* into FONT.
Return remaining LENGTH.")
    (:method (length font)
      "Parse the 7 regular FONT parameters. Return remaining LENGTH."
      (when (>= length 1)
	(setf (slot-value font 'slant) (read-fix-word nil))
	(decf length))
      (map-font-dimension-slots slot font read-parameter)
      length)
    (:method (length (font math-symbols-font))
      "Parse the 15 additional TeX math symbols FONT parameters."
      (setq length (call-next-method))
      (map-math-symbols-font-dimension-slots slot font read-parameter)
      length)
    (:method (length (font math-extension-font))
      "Parse the 6 additional TeX math extension FONT parameters."
      (setq length (call-next-method))
      (map-math-extension-font-dimension-slots slot font read-parameter)
      length)
    (:method :around (length font)
      "Read remaining parameters into a parameters array."
      (setq length (call-next-method))
      (unless (zerop length)
	(let ((array (make-array length)))
	  (loop :for i :from 0 :upto (1- length)
		:do (setf (aref array i) (read-fix-word)))
	  (setf (slot-value font 'parameters) array))))))




;; ==========================================================================
;; Preamble
;; ==========================================================================

(define-condition file-size-mixin ()
  ((declared-size
    :documentation "The declared file size, in bytes."
    :initarg :declared-size
    :reader declared-size)
   (actual-size
    :documentation "The actual file size, in bytes."
    :initarg :actual-size
    :reader actual-size))
  (:documentation "The File Size Mixin condition.
It is used in both errors and warnings to report different declared and
actual file sizes."))

(define-condition file-underflow (file-size-mixin tfm-compliance-error)
  ((reference :initform 8)) ; slot merge
  (:documentation "The File Underflow compliance error.
It signals that the file size is shorter than expected."))

(define-condition-report (condition file-underflow)
    "declared file size ~A is greater than actual ~A bytes"
  (declared-size condition)
  (actual-size condition))


;; #### NOTE: this one is a warning instead of an error because TeX silently
;; ignores junk at the end of TFM files (see TeX: the Program [575]). We hence
;; do the same, but still signal a warning.
(define-condition file-overflow (file-size-mixin tfm-compliance-warning)
  ((reference :initform 8)) ; slot merge
  (:documentation "The File Overflow compliance warning.
It signals that the file size is longer than expected."))

(define-condition-report (condition file-overflow)
    "declared file size ~A is less than actual ~A bytes"
  (declared-size condition)
  (actual-size condition))


(define-condition invalid-header-length (tfm-compliance-error)
  ((reference :initform 8) ; slot merge
   (value
    :documentation "The invalid header length."
    :initarg :value
    :reader value))
  (:documentation "The Invalid Header Length compliance error.
It signals that a header length is too small (< 2 words)."))

(define-condition-report (condition invalid-header-length)
    "~A word~:P header length is too small (< 2 words)"
  (value condition))


(define-condition invalid-character-range (tfm-compliance-error)
  ((reference :initform 8) ; slot merge
   (bc :documentation "The smallest character code." :initarg :bc :reader bc)
   (ec :documentation "The largest character code." :initarg :ec :reader ec))
  (:documentation "The Invalid Character Range compliance error.
It signals that BC-1 > EC, or that EC > 255."))

(define-condition-report (condition invalid-character-range)
    "character range ~A (bc) - ~A (ec) doesn't satisfy bc-1 <= ec && ec <= 255)"
  (bc condition)
  (ec condition))


(define-condition invalid-table-length (tfm-table-error)
  ((reference :initform 11) ; slot merge
   (value
    :documentation "The invalid table length."
    :initarg :value
    :accessor value)
   (smallest
    :documentation "The smallest table length."
    :initarg :smallest
    :accessor smallest)
   (largest
    :documentation "The largest table length."
    :initarg :largest
    :accessor largest))
  (:documentation "The Invalid Table Length compliance error.
It signals that a declared TFM table's length is out of range."))

(define-condition-report (condition invalid-table-length)
  "~A table length ~A is invalid (should be in [~A,~A])"
  (name condition)
  (value condition)
  (smallest condition)
  (largest condition))




;; ==========================================================================
;; Stream Loading
;; ==========================================================================

(define-condition invalid-section-lengths (tfm-compliance-error)
  ((reference :initform 8) ; slot merge
   (lf
    :documentation "The declared length of the file."
    :initarg :lf
    :reader lf)
   (lh
    :documentation "The declared length of the file header."
    :initarg :lh
    :reader lh)
   (nc
    :documentation "EC - BC + 1."
    :initarg :nc
    :reader nc)
   (nw
    :documentation "The declared length of the width table."
    :initarg :nw
    :reader nw)
   (nh
    :documentation "The declared length of the height table."
    :initarg :nh
    :reader nh)
   (nd
    :documentation "The declared length of the depth table."
    :initarg :nd
    :reader nd)
   (ni
    :documentation "The declared length of the italic correction table."
    :initarg :ni
    :reader ni)
   (nl
    :documentation "The declared length of the lig/kern table."
    :initarg :nl
    :reader nl)
   (nk
    :documentation "The declared length of the kern table."
    :initarg :nk
    :reader nk)
   (ne
    :documentation "The declared length of the extensible character table."
    :initarg :ne
    :reader ne)
   (np
    :documentation "The declared length of the parameters section."
    :initarg :np
    :reader np))
  (:documentation "The Section Lengths compliance error.
It signals that LF != 6 + LH + NC + NW + NH + ND + NI + NL + NK + NE + NP."))

(define-condition-report (condition invalid-section-lengths)
    "section lengths don't satisfy ~
~A (lf) = 6 + ~A (lh) + ~A (nc) + ~A (nw) + ~A (nh) + ~A (nd) + ~A (ni) ~
+ ~A (nl) + ~A (nk) + ~A (ne) + ~A (np)"
  (lf condition)
  (lh condition)
  (nc condition)
  (nw condition)
  (nh condition)
  (nd condition)
  (ni condition)
  (nl condition)
  (nk condition)
  (ne condition)
  (np condition))


(define-condition invalid-ofm0-section-lengths (invalid-section-lengths)
  ((reference :initform '(:omega . 7.1))) ; slot merge
  (:documentation "The Invalid Level 0 OFM Section Lengths compliance error.
It signals that
LF != 14 + LH + 2*NC + NW + NH + ND + NI + 2*NL + NK + 2*NE + NP."))

(define-condition-report (condition invalid-ofm0-section-lengths)
  "section lengths don't satisfy ~
~A (lf) = 14 + ~A (lh) + 2*~A (nc) + ~A (nw) + ~A (nh) + ~A (nd) + ~A (ni) ~
+ 2*~A (nl) + ~A (nk) + 2*~A (ne) + ~A (np)"
  (lf condition)
  (lh condition)
  (nc condition)
  (nw condition)
  (nh condition)
  (nd condition)
  (ni condition)
  (nl condition)
  (nk condition)
  (ne condition)
  (np condition))


(defun load-stream
    (font lf &aux (fmt (etypecase font (ofm0-font :ofm0) (font :tfm))))
  "Parse *STREAM* of declared length LF into FONT, and return it.

If *STREAM* is shorter than expected, signal a FILE-UNDERFLOW error.
If *STREAM* is longer than expected, signal a FILE-OVERFLOW warning.

If the declared header length is less than 2, signal an INVALID-HEADER-LENGTH
error.

If BC and EC don't make sense, signal an INVALID-CHARACTER-RANGE error.

If the widths, heights, depths, italic corrections, or extens tables lengths
are not within the expected range, signal an INVALID-TABLE-LENGTH error.

Finally, if the declared sections lengths don't add up to the declared file
length, signal an INVALID-[OFM0-]SECTION-LENGTHS error."

  ;; 1. Read the rest of the preamble and perform some sanity checks.
  ;; #### NOTE: the errors signalled below (directly, or by the WORD-READER)
  ;; are really too early to attempt any clever recovery.
  (let* ((word-reader (ecase fmt (:ofm0 #'read-u32) (:tfm #'read-u16)))
	 (lh (funcall word-reader))
	 (bc (funcall word-reader))
	 (ec (funcall word-reader))
	 (nw (funcall word-reader))
	 (nh (funcall word-reader))
	 (nd (funcall word-reader))
	 (ni (funcall word-reader))
	 (nl (funcall word-reader))
	 (nk (funcall word-reader))
	 (ne (funcall word-reader))
	 (np (funcall word-reader))
	 (fd (when (eq fmt :ofm0) (funcall word-reader)))
	 nc)
    (let ((actual-size (file-length *stream*))
	  (declared-size (* 4 lf)))
      (when actual-size
	(cond ((< actual-size declared-size)
	       (error 'file-underflow
		 :actual-size actual-size
		 :declared-size declared-size))
	      ((> actual-size declared-size)
	       ;; #### NOTE: we could collect the actual junk at the end of
	       ;; parsing and report it, but it doesn't seem to be worth it.
	       ;; I've looked at a couple of concerned files and there doesn't
	       ;; seem to be any kind of interesting information in there
	       ;; (contrary to padded strings).
	       (warn 'file-overflow
		 :actual-size actual-size
		 :declared-size declared-size)))))
    (unless (>= lh 2) (error 'invalid-header-length :value lh))
    (unless (and (<= (1- bc) ec) (<= ec (if (eq fmt :ofm0) 65535 255)))
      (error 'invalid-character-range :bc bc :ec ec))
    (setq nc (+ ec (- bc) 1))
    (unless (zerop nc)
      (setf (slot-value font 'min-code) bc
	    (slot-value font 'max-code) ec))
    (loop :for length :in (list nw nh nd ni ne)
	  :for min :in '(1 1 1 1 0)
	  :for max :in (if (eq fmt :ofm0)
			 '(65536 256 256 256 256)
			 '(256 16 16 64 256))
	  :for name :in '("widths" "heights" "depths" "italic corrections"
			  "extens")
	  :unless (<= min length max)
	    :do (error 'invalid-table-length
		  :value length :smallest min :largest max :name name))
    (unless (= lf (if (eq fmt :ofm0)
		    (+ 14 lh (* 2 nc) nw nh nd ni (* 2 nl) nk (* 2 ne) np)
		    (+ 6 lh nc nw nh nd ni nl nk ne np)))
      (error (if (eq fmt :ofm0)
	       'invalid-ofm0-section-lengths
	       'invalid-section-lengths)
	:lf lf :lh lh :nc nc :nw nw :nh nh :nd nd :ni ni :nl nl :nk nk
	:ne ne :np np))
    (when (eq fmt :ofm0)
      (let ((direction (case (mod fd 8)
			 (0 :top-left)
			 (1 :left-top)
			 (2 :top-right)
			 (3 :left-bottom)
			 (4 :bottom-left)
			 (5 :right-top)
			 (6 :bottom-right)
			 (7 :right-bottom))))
	(setf (slot-value font 'direction)
	      (if (>= fd 7) (cons :natural direction) direction))))

    ;; 2. Parse the header section.
    (parse-header lh font)

    ;; 3. Parse the 8 character-related sections.
    (parse-character-information nc nw nh nd ni nl nk ne font)

    ;; 4. Parse the parameters section.
    (parse-parameters np font))

  font)




;; ==========================================================================
;; Entry Point
;; ==========================================================================

(define-condition invalid-custom-name (tfm-usage-error)
  ((name
    :documentation "The invalid custom name."
    :initarg :name
    :reader name))
  (:documentation "The Invalid Custom Name usage error.
It signals that a custom name is not a non-empty string."))

(define-condition-report (condition invalid-custom-name)
    "custom name ~S is invalid (should be a non-empty string)"
  (name condition))


(define-condition unsupported-format (tfm-warning)
  ((fmt
    :documentation "The unsupported format.
Possible values include :OFM1 (meaning level 1 OFM data) and :JFM."
    :initarg :fmt
    :reader fmt)
   ;; #### NOTE: this slot is here for completeness, but since context-aware
   ;; condition reporting already mentions the file we're reading from, it's
   ;; not used in the condition reporting code below.
   (file
    :documentation "The unsupported font's file name."
    :initarg :file
    :reader file))
  (:documentation "The Unsupported Format warning.
It signals that a file contains data in a currently unsupported format.
Currently supported font formats are regular TFM and level 0 Omega (OFM).
Level 1 OFM and JFM formats might be supported in the future."))

(define-condition-report (condition unsupported-format)
    "~A data is not supported yet"
  (ecase (fmt condition)
    (:ofm1 "level 1 Omega (OFM)")
    (:jfm "JFM")))


(define-condition invalid-ofm-level (tfm-compliance-error)
  ((value :documentation "The invalid level." :initarg :value :accessor value))
  (:documentation "The Invalid OFM LEVEL compliance error.
It signals that an OFM font advertises a level different from 0 or 1."))

(define-condition-report (condition invalid-ofm-level)
  "OFM level ~S is invalid (should be 0 or 1)"
  (value condition))


(defun load-font (file &rest keys &key name design-size freeze &aux lf font)
  "Load FILE into a new font, and return it.
- FILE must be a pathname designator.
- If NAME is not NIL, use it as the font's name instead of FILE's base name,
  in which case it must be a non-empty string. Otherwise, signal an
  INVALID-CUSTOM-NAME error. This error is immediately restartable with
  USE-FILE-BASE-NAME.
- If DESIGN-SIZE is not NIL, use it as the font's design size instead of the
  original one, in which case it must be a real greater or equal to 1.
  Otherwise, signal an INVALID-CUSTOM-DESIGN-SIZE error. This error is
  immediately restartable with USE-ORIGINAL-DESIGN-SIZE.
- When FREEZE (NIL by default), freeze the font immediately after loading it.
  See the eponymous function for more information.

Both regular TFM and level 0 Omega (OFM) fonts are currently supported. If
level 1 OFM or JFM data is detected, this function signals an
UNSUPPORTED-FORMAT warning and returns NIL.

Any condition signalled while FILE is being loaded is restartable with
CANCEL-LOADING, in which case this function simply returns NIL."
  (setq keys (remove-keys keys :freeze))
  (when name
    (unless (and (stringp name) (not (zerop (length name))))
      (restart-case (error 'invalid-custom-name :name name)
	(use-file-base-name () :report "Use the font file's base name."
	  (setq keys (remove-keys keys :name))))))
  (when design-size
    (unless (typep design-size '(real 1))
      (restart-case (error 'invalid-custom-design-size :value design-size)
	(use-original-design-size ()
	  :report "Use the font's original design size."
	  (setq keys (remove-keys keys :design-size))))))
  (with-open-file
      (*stream* file :direction :input :element-type '(unsigned-byte 8))
    ;; #### NOTE: in order to detect the format, we don't even know how many
    ;; bytes to read at first. TFM requires 2 but OFM requires 4. So we cannot
    ;; perform any early checking on the first two bytes.
    (setq lf (read-u16 nil))
    (case lf
      (0
       (setq lf (read-u16 nil))
       (case lf
	 (0
	  (with-simple-restart (cancel-loading "Cancel loading this font.")
	    (setq lf (read-u32))
	    (setq font (load-stream
			(apply #'make-instance 'ofm0-font :file file keys)
			lf))))
	 (1
	  (warn 'unsupported-format :fmt :ofm1 :file file))
	 (t
	  (with-simple-restart (cancel-loading "Cancel loading this font.")
	    (error 'invalid-ofm-level :value lf)))))
      ((9 11)
       (warn 'unsupported-format :fmt :jfm :file file))
      (t
       (with-simple-restart (cancel-loading "Cancel loading this font.")
	 (unless (zerop (ldb (byte 1 15) lf)) (error 'u16-overflow :value lf))
	 (setq font (load-stream
		     (apply #'make-instance 'font :file file keys)
		     lf))))))
  (when (and font freeze) (freeze font))
  font)

;;; file.lisp ends here
