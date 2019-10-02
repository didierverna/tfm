;;; file.lisp --- Parsing and decoding

;; Copyright (C) 2018, 2019 Didier Verna

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



;;; Code:

(in-package :net.didierverna.tfm)
(in-readtable :net.didierverna.tfm)


;; ==========================================================================
;; Header
;; ==========================================================================

(define-condition invalid-design-size (tfm-compliance-error)
  ((value
    :documentation "The invalid design size."
    :initarg :value
    :accessor value))
  (:report (lambda (invalid-design-size stream)
	     (report stream "~Apt~:P design size is too small (< 1pt)."
	       (value invalid-design-size))))
  (:documentation "The Invalid Design Size compliance error.
It signals that a design size is too small (< 1pt)."))

(defun parse-header (length font)
  "Parse a header of LENGTH words from *STREAM* into FONT.
If FONT's design size is less than 1pt, signal an INVALID-DESIGN-ERROR. This
error is immediately restartable with SET-TO-TEN."
  ;; #### NOTE: LENGTH >= 2 has already been checked by the caller,
  ;; LOAD-TFM-FONT.
  (setf (checksum font) (read-u32)
	(design-size font) (read-fix-word nil))
  (unless (>= (design-size font) 1)
    (restart-case (error 'invalid-design-size :value (design-size font))
      (set-to-ten () :report "Set it to 10pt." (setf (design-size font) 10))))
  (decf length 2)
  ;; #### NOTE: we silently assume Xerox PARC headers below. Not sure if
  ;; anything else could be in use, but it's impossible to tell from the files
  ;; themselves. TeX Live 2019 doesn't seem to have anything else.
  #i(checking-length 1)
  (macrolet ((checking-length (needed &body body)
	       "Execute BODY if LENGTH >= NEEDED.
If so, decrease LENGTH by NEEDED afterwards."
	       `(when (>= length ,needed) ,@body (decf length ,needed))))
    (checking-length 10 (setf (encoding font) (read-padded-string 40)))
    (when (encoding font)
      ;; #### NOTE: we don't upcase the BCPL strings, but tftopl does, so it's
      ;; probably better to do string comparisons on upcased versions. Also,
      ;; tftopl only checks for SY and EX, not the full strings, but I don't
      ;; think that really matters.
      (cond ((string= (string-upcase (encoding font)) "TEX MATH SYMBOLS")
	     (change-class font 'math-symbols-font))
	    ((string= (string-upcase (encoding font)) "TEX MATH EXTENSION")
	     (change-class font 'math-extension-font))))
    (checking-length  5 (setf (family font) (read-padded-string 20)))
    (checking-length  1
      (let ((word (read-u32)))
	(setf (7bits-safe font) (ldb (byte 1 31) word))
	(let ((face (ldb (byte 8 0) word)))
	  (setf (face-number font) face)
	  (when (< face 18)
	    (setf (face-code font) (make-string 3))
	    (case face
	      ((0 1  6  7 12 13)
	       (setf (weight font) :medium (aref (face-code font) 0) #\M))
	      ((2 3  8  9 14 15)
	       (setf (weight font) :bold (aref (face-code font) 0) #\B))
	      ((4 5 10 11 16 17)
	       (setf (weight font) :light (aref (face-code font) 0) #\L)))
	    (case face
	      ((0 2 4 6 8 10 12 14 16)
	       (setf (slope font) :roman (aref (face-code font) 1) #\R))
	      ((1 3 5 7 9 11 13 15 17)
	       (setf (slope font) :bold (aref (face-code font) 1) #\I)))
	    (case face
	      ((0  1  2  3  4  5)
	       (setf (expansion font) :regular (aref (face-code font) 2) #\R))
	      ((6  7  8  9  10 11)
	       (setf (expansion font) :condensed (aref (face-code font) 2) #\C))
	      ((12 13 14 15 16 17)
	       (setf (expansion font) :extended
		     (aref (face-code font) 2) #\E))))))))
  (loop :repeat length :do (read-u32)))



;; ==========================================================================
;; Character Information Tables
;; ==========================================================================

(define-condition tfm-table-error (tfm-compliance-error)
  ((name :documentation "The table's name." :initarg :name :accessor name))
  (:documentation "The TFM table errors root condition.
This is the root condition for errors related to TFM tables."))

(define-condition invalid-table-index (tfm-table-error)
  ((value
    :documentation "The invalid index."
    :initarg :value
    :accessor value)
   (largest
    :documentation "The largest index."
    :initarg :largest
    :accessor largest))
  (:report (lambda (invalid-table-index stream)
	     (report stream "index ~A in ~A table is invalid (largest is ~A)."
	       (value invalid-table-index)
	       (name invalid-table-index)
	       (largest invalid-table-index))))
  (:documentation "The Invalid Table Index compliance error.
It signals that a table index is greater than its largest value."))

(defun table-aref (name table index)
  "Access NAMEd TABLE at INDEX.
If INDEX is out of bounds, signal an INVALID-TABLE-INDEX error."
  (unless (< index (length table))
    (error 'invalid-table-index
      :value index :largest (1- (length table)) :name name))
  (aref table index))

(defmacro tref (table index)
  "Call TABLE-AREF, computing the table name from TABLE."
  `(table-aref ,(string-downcase (symbol-name table)) ,table ,index))


;; -------------------------
;; Ligature/Kerning Programs
;; -------------------------

(define-condition invalid-ligature-opcode (tfm-compliance-error)
  ((value
    :documentation "The invalid ligature opcode."
    :initarg :value
    :accessor value))
  (:report (lambda (invalid-ligature-opcode stream)
	     (report stream "ligature opcode ~A is invalid."
	       (value invalid-ligature-opcode))))
  (:documentation "The Invalid Ligature Opcode compliance error.
It signals that a ligature opcode is invalid."))

(defun %run-ligature/kerning-program
    (character index lig/kerns kerns &aux (font (font character)))
  "Run a ligature/kerning program for CHARACTER.
The program starts at LIG/KERNS[INDEX] and uses the KERNS array. Running the
program eventually creates ligatures or kernings for CHARACTER and some other
character.

If an invalid index into LIG/KERNS is encountered, signal an
INVALID-TABLE-INDEX error. This error is immediately restartable with
ABORT-LIG/KERN-PROGRAM.

If an invalid ligature opcode is encountered, signal an
INVALID-LIGATURE-OPCODE error. This error is immediately restartable with
DISCARD-LIGATURE.

If an invalid index into KERNS is encountered, signal an INVALID-TABLE-INDEX
error. This error is immediately restartable with DISCARD-KERNING."
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
	      (if (or (= opcode 4) (and (> opcode 7) (not (= opcode 11))))
		(with-simple-restart
		    (discard-ligature "Discard this ligature instruction.")
		  (error 'invalid-ligature-opcode :value opcode))
		(setf (ligature character (code-character (next lig/kern) font))
		      (make-ligature
		       (code-character (remainder lig/kern) font)
		       (when (member opcode '(0 1 5)) t)
		       (when (member opcode '(0 2 6)) t)
		       (cond ((member opcode '(0 1 2 3)) 0)
			     ((member opcode '(5 6 7)) 1)
			     ((= opcode 11) 2))))))
	    ;; kerning instruction
	    (with-simple-restart
		(discard-kerning "Discard this kerning instruction.")
	      (setf (kerning character (code-character (next lig/kern) font))
		    (tref kerns (+ (* 256 (- (op lig/kern) 128))
				   (remainder lig/kern))))))
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
       (+ (* 256 (op lig/kern)) (remainder lig/kern))
       index)
     lig/kerns
     kerns)))


;; -----------------
;; Extension Recipes
;; -----------------

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

(define-condition invalid-char-info (tfm-compliance-error)
  ((value
    :documentation "The invalid char-info structure."
    :initarg :value
    :accessor value))
  (:report (lambda (invalid-char-info stream)
	     (report stream
		 "~A is invalid (should be 0 0 0 0 NIL NIL NIL)."
	       (value invalid-char-info))))
  (:documentation "The Invalid Char Info compliance error.
It signals that a char-info with a width-index of 0 is not completely
zero'ed out."))

(define-condition invalid-table-start (tfm-table-error)
  ((value
    :documentation "The invalid first table value."
    :initarg :value
    :accessor value))
  (:report (lambda (invalid-table-start stream)
	     (report stream
		 "first value ~A in ~A table is invalid (should be 0)."
	       (value invalid-table-start)
	       (name invalid-table-start))))
  (:documentation "The Invalid Table Start compliance error.
It signals that the first value in a TFM table is not 0."))

(define-condition no-boundary-character (tfm-compliance-error)
  ()
  (:report (lambda (no-boundary-character stream)
	     (declare (ignore no-boundary-character))
	     (report stream
		 "found a boundary character ligature/kerning program,~
without a boundary character being defined.")))
  (:documentation "The No Boundary Character compliance error.
It signals that a boundary character ligature/kerning program was found,
without a boundary character being defined."))

(define-condition character-list-cycle (tfm-compliance-error)
  ((value
    :documentation "The cyclic character list."
    :initarg :value
    :accessor value))
  (:report (lambda (character-list-cycle stream)
	     (report stream "found a cycle in character list ~A."
	       (value character-list-cycle))))
  (:documentation "The Character List Cycle compliance error.
It signals that a cycle was found in a list of ascending character sizes."))

(define-condition ligature-cycle (tfm-compliance-error)
  ((value
    :documentation "The ligature introducing a cycle."
    :initarg :value
    :accessor value)
   (characters
    :documentation "The cons of characters involved in the ligature."
    :initarg :characters
    :accessor characters))
  (:report (lambda (ligature-cycle stream)
	     (report stream
		 "ligature ~A introduces a cycle for characters ~A."
	       (value ligature-cycle)
	       (characters ligature-cycle))))
  (:documentation "The Ligature Cycle compliance error.
It signals that a ligature introduces a cycle for a cons of characters."))

(defun parse-character-information (nc nw nh nd ni nl nk ne font)
  "Parse the 8 character information tables from *STREAM* into FONT.
NC (EC - BC + 1), NW, NH, ND, NI, NL, NK, and NE are the declared lengths of
the 8 tables, that is, the char infos, widths, heights, depths, italic
corrections, lig/kern instructions, kerns, and extens respectively.

If a char info structure with a width index of 0 is not completely zero'ed
out, signal an INVALID-CHAR-INFO error. This error is immediately restartable
with SET-TO-ZERO.

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
BREAK-CYCLE.

If a ligature is found to be cyclic, signal a LIGATURE-CYCLE error. This error
is immediately restartable with REMOVE-LIGATURE."
  (let ((char-infos (make-array nc :fill-pointer 0))
	(widths (make-array nw :fill-pointer 0))
	(heights (make-array nh :fill-pointer 0))
	(depths (make-array nd :fill-pointer 0))
	(italics (make-array ni :fill-pointer 0))
	(lig/kerns (make-array nl :fill-pointer 0))
	(kerns (make-array nk :fill-pointer 0))
	(extens (make-array ne :fill-pointer 0)))

    ;; 1. Read the tables.
    (loop :repeat nc
	  :for word = (read-u32)
	  :for char-info := (decode-char-info word)
	  :unless (or (not (zerop (width-index char-info))) (zerop word))
	    :do (restart-case (error 'invalid-char-info :value char-info)
		  (set-to-zero () :report "Zero it out."
		    (setq char-info (decode-char-info 0))))
	  :do (vector-push char-info char-infos))
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
	  :do (loop :repeat (1- length)
		    :do (vector-push (read-fix-word) array)))
    (loop :repeat nl
	  :do (vector-push (decode-lig/kern (read-u32)) lig/kerns))
    (loop :repeat nk
	  :do (vector-push (read-fix-word) kerns))
    (loop :repeat ne
	  :do (vector-push (decode-exten (read-u32)) extens))

    ;; 2. Create the character metrics.
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
			   0)))))
    ;; #### NOTE: this count doesn't (and shouldn't) include a zero'ed out
    ;; boundary character potentially added below.
    (setf (character-count font) (hash-table-count (characters font)))

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
	    (setf (boundary-character font)
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
	     (+ (* 256 (op lig/kern)) (remainder lig/kern))
	     lig/kerns
	     kerns)
	    (with-simple-restart
		(abort-lig/kern-program "Abort the ligature/kerning program.")
	      (error 'no-boundary-character))))))

    ;; 4. Process ligature / kerning programs, character lists, and extension
    ;; recipes, character by character.
    (loop :for char-info :across char-infos
	  :for code :from (min-code font)
	  ;; #### NOTE: technically, this check is not needed because Step 2
	  ;; made sure that such an (inexistent) character is completely
	  ;; zero'ed out. But it's cleaner and more explicit to keep it, plus
	  ;; it avoids the 3 useless COND checks below.
	  :unless (zerop (width-index char-info))
	    :do (cond ((lig/kern-index char-info)
		       (run-ligature/kerning-program
			(code-character code font)
			(lig/kern-index char-info)
			lig/kerns
			kerns))
		      ((next-char char-info)
		       (setf (next-character (code-character code font))
			     (code-character (next-char char-info) font)))
		      ((exten-index char-info)
		       (with-simple-restart
			   (discard-extension-recipe
			    "Discard this extension recipe.")
			 (setf (extension-recipe (code-character code font))
			       (font-extension-recipe
				(tref extens (exten-index char-info))
				font)))))))

  ;; #### NOTE: we're done with the tables now.

  ;; #### WARNING: the two checks below have not been tested thoroughly. They
  ;; #### have been applied to all fonts in TeX Live, but not on fonts made
  ;; #### explicitly to contain cycles, so we're not really sure that they
  ;; #### work.

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
			       (error 'character-list-cycle :value seen)
			     (break-cycle () :report "Break the cycle."
			       (setf (next-character character) nil)))
		     :else
		       :do (push (next-character character) seen)
		       :and :do (setq character
				      (next-character character)))))
	   (characters font))

  ;; 6. Check for ligature cycles, ligature by ligature. Again, this is
  ;; perhaps not the most efficient way to do it and maybe I should study the
  ;; algorithm used in TFtoPL[88..], but we don't care right now.
  (maphash (lambda (characters first-ligature)
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
				    :value first-ligature
				    :characters characters)
				(remove-ligature ()
				  :report "Remove that ligature from the font."
				  (remhash characters (ligatures font))
				  (setq ligature nil))))
			     (t
			      (push state seen)
			      (setq ligature
				    (ligature (car state) (cadr state)))))))
	   (ligatures font)))



;; ==========================================================================
;; Parameters Section
;; ==========================================================================

;; #### WARNING: this local macro intentionally captures LENGTH and FONT!
(macrolet ((read-parameter (parameter)
	     "Read a fix word into font's PARAMETER if length >= 1.
If so, decrement length afterwards."
	     `(when (>= length 1)
		(setf (,parameter font) (read-fix-word))
		(decf length)))
	   (read-parameters (&rest parameters)
	     "Read fix words into font's PARAMETERS, length permitting.
Decrement length accordingly afterwards."
	     `(progn ,@(mapcar (lambda (parameter)
				 (list 'read-parameter parameter))
			 parameters))))
  (defgeneric parse-parameters (length font)
    (:documentation
     "Parse a parameters section of LENGTH words from *STREAM* into FONT.
Return remaining LENGTH.")
    (:method (length font)
      "Parse the 7 regular FONT parameters. Return remaining LENGTH."
      (when (>= length 1)
	(setf (slant font) (read-fix-word nil))
	(decf length))
      (read-parameters interword-space interword-stretch interword-shrink
		       ex em
		       extra-space)
      length)
    (:method (length (font math-symbols-font))
      "Parse the 15 additional TeX math symbols FONT parameters."
      (setq length (call-next-method))
      (read-parameters num1 num2 num3
		       denom1 denom2
		       sup1 sup2 sup3
		       sub1 sub2
		       subdrop supdrop
		       delim1 delim2
		       axis-height)
      length)
    (:method (length (font math-extension-font))
      "Parse the 6 additional TeX math extension FONT parameters."
      (setq length (call-next-method))
      (read-parameters default-rule-thickness
		       big-op-spacing1 big-op-spacing2 big-op-spacing3
		       big-op-spacing4 big-op-spacing5)
      length)
    (:method :around (length font)
      "Read remaining parameters into a parameters array."
      (setq length (call-next-method))
      (unless (zerop length)
	(let ((array (make-array length)))
	  (loop :for i :from 0 :upto (1- length)
		:do (setf (aref array i) (read-fix-word)))
	  (setf (parameters font) array))))))



;; ==========================================================================
;; Preamble
;; ==========================================================================

(define-condition file-size-mixin ()
  ((declared-size
    :documentation "The declared file size."
    :initarg :declared-size
    :accessor declared-size)
   (actual-size
    :documentation "The actual file size."
    :initarg :actual-size
    :accessor actual-size))
  (:documentation "The File Size Mixin condition.
It is used in both errors and warnings to report different declared and
actual file sizes."))

(define-condition file-underflow (file-size-mixin tfm-compliance-error)
  ()
  (:report (lambda (file-underflow stream)
	     (report stream
		 "actual file size ~A is lesser than declared one ~A."
	       (actual-size file-underflow)
	       (declared-size file-underflow))))
  (:documentation "The File Underflow compliance error.
It signals that the file size is shorter than expected."))

;; #### NOTE: this one is a warning instead of an error because TeX silently
;; ignores junk at the end of TFM files (see TeX: the Program [575]). We hence
;; do the same, but still signal a warning.
(define-condition file-overflow (file-size-mixin tfm-compliance-warning)
  ()
  (:report (lambda (file-overflow stream)
	     (report stream
		 "declared file size ~A is lesser than actual one ~A."
	       (declared-size file-overflow)
	       (actual-size file-overflow))))
  (:documentation "The File Overflow compliance warning.
It signals that the file size is longer than expected."))

(define-condition invalid-header-length (tfm-compliance-error)
  ((value
    :documentation "The invalid header length."
    :initarg :value
    :accessor value))
  (:report (lambda (invalid-header-length stream)
	     (report stream
		 "~A word~:P header length is too small (< 2 words)."
	       (value invalid-header-length))))
  (:documentation "The Invalid Header Length compliance error.
It signals that a header length is too small (< 2 words)."))

(define-condition invalid-character-range (tfm-compliance-error)
  ((bc :documentation "The smallest character code." :initarg :bc :accessor bc)
   (ec :documentation "The largest character code." :initarg :ec :accessor ec))
  (:report (lambda (invalid-character-range stream)
	     (report stream "~
character range ~A (bc) - ~A (ec) doesn't satisfy bc-1 <= ec && ec <= 255)."
	       (bc invalid-character-range)
	       (ec invalid-character-range))))
  (:documentation "The Invalid Character Range compliance error.
It signals that BC-1 > EC, or that EC > 255."))

(define-condition invalid-section-lengths (tfm-compliance-error)
  ((lf
    :documentation "The declared length of the file."
    :initarg :lf
    :accessor lf)
   (lh
    :documentation "The declared length of the file header."
    :initarg :lh
    :accessor lh)
   (nc
    :documentation "EC - BC + 1."
    :initarg :nc
    :accessor nc)
   (nw
    :documentation "The declared length of the width table."
    :initarg :nw
    :accessor nw)
   (nh
    :documentation "The declared length of the height table."
    :initarg :nh
    :accessor nh)
   (nd
    :documentation "The declared length of the depth table."
    :initarg :nd
    :accessor nd)
   (ni
    :documentation "The declared length of the italic correction table."
    :initarg :ni
    :accessor ni)
   (nl
    :documentation "The declared length of the lig/kern table."
    :initarg :nl
    :accessor nl)
   (nk
    :documentation "The declared length of the kern table."
    :initarg :nk
    :accessor nk)
   (ne
    :documentation "The declared length of the extensible character table."
    :initarg :ne
    :accessor ne)
   (np
    :documentation "The declared length of the parameters section."
    :initarg :np
    :accessor np))
  (:report (lambda (section-lengths stream)
	     (report stream "~
section lengths don't satisfy ~
~A (lf) = 6 + ~A (lh) + ~A (nc) + ~A (nw) + ~A (nh) + ~A (nd) + ~A (ni) ~
+ ~A (nl) + ~A (nk) + ~A (ne) + ~A (np)."
	       (lf section-lengths)
	       (lh section-lengths)
	       (nc section-lengths)
	       (nw section-lengths)
	       (nh section-lengths)
	       (nd section-lengths)
	       (ni section-lengths)
	       (nl section-lengths)
	       (nk section-lengths)
	       (ne section-lengths)
	       (np section-lengths))))
  (:documentation "The Section Lengths compliance error.
It signals that LF != 6 + LH + NC + NW + NH + ND + NI + NL + NK + NE + NP."))

(define-condition invalid-table-length (tfm-table-error)
  ((value
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
  (:report (lambda (invalid-table-length stream)
	     (report stream "~
~A table length ~A is invalid (should be in [~A,~A])."
	       (name invalid-table-length)
	       (value invalid-table-length)
	       (smallest invalid-table-length)
	       (largest invalid-table-length))))
  (:documentation "The Invalid Table Length compliance error.
It signals that a declared TFM table's length is out of range."))

(defun load-tfm-font (lf
		      &key (file (when (typep *stream* 'file-stream)
				   (pathname *stream*)))
			   (name (when file
				   (pathname-name file)))
		      &aux (font (make-font name :file file)))
  "Parse *STREAM* of declared length LF into a new font, and return it.
FILE defaults to *STREAM*'s associated file if any, and NAME defaults to
the FILE's base name, if any.

If *STREAM* is shorter than expected, signal a FILE-UNDERFLOW error.
If *STREAM* is longer than expected, signal a FILE-OVERFLOW warning.

If the declared header length is less than 2, signal an INVALID-HEADER-LENGTH
error.

If BC and EC don't make sense, signal an INVALID-CHARACTER-RANGE error.

If the widths, heights, depths, italic corrections, or extens tables lengths
are not within the expected range, signal an INVALID-TABLE-LENGTH error.

Finally, if the declared sections lengths don't add up to the declared file
length, signal an INVALID-SECTION-LENGTHS error."

  ;; 1. Read the rest of the preamble and perform some sanity checks.
  ;; #### NOTE: the errors signalled below (directly, or by READ-U16) are
  ;; really too early to attempt any clever recovery.
  (let ((lh (read-u16))
	(bc (read-u16))
	(ec (read-u16))
	(nw (read-u16))
	(nh (read-u16))
	(nd (read-u16))
	(ni (read-u16))
	(nl (read-u16))
	(nk (read-u16))
	(ne (read-u16))
	(np (read-u16))
	nc)
    (let ((actual-size (file-length *stream*))
	  (declared-size (* 4 lf)))
      (when actual-size
	(cond ((< actual-size declared-size)
	       (error 'file-underflow
		 :actual-size actual-size
		 :declared-size declared-size))
	      ((> actual-size declared-size)
	       (warn 'file-overflow
		     :actual-size actual-size
		     :declared-size declared-size)))))
    (unless (>= lh 2) (error 'invalid-header-length :value lh))
    (unless (and (<= (1- bc) ec) (<= ec 255))
      (error 'invalid-character-range :bc bc :ec ec))
    (when (> bc 255) (setq bc 1 ec 0))
    (setq nc (+ ec (- bc) 1))
    (setf (min-code font) bc (max-code font) ec)
    (loop :for length :in (list nw nh nd ni ne)
	  :for min :in '(1 1 1 1 0)
	  :for max :in '(256 16 16 64 256)
	  :for name :in '("widths" "heights" "depths" "italic corrections"
			  "extens")
	  :unless (<= min length max)
	    :do (error 'invalid-table-length
		       :value length :smallest min :largest max :name name))
    (unless (= lf (+ 6 lh nc nw nh nd ni nl nk ne np))
      (error 'invalid-section-lengths
	     :lf lf :lh lh :nc nc :nw nw :nh nh :nd nd :ni ni :nl nl :nk nk
	     :ne ne :np np))

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

(define-condition extended-tfm (tfm-warning)
  ((file :documentation "The extended TFM file." :initarg :file :accessor file)
   (value :documentation "The TFM extension." :initarg :value :accessor value))
  (:report (lambda (extended-tfm stream)
	     (format stream "File ~A contains ~A data (not supported yet)."
	       (file extended-tfm)
	       (value extended-tfm))))
  (:documentation "The Extended TFM warning.
It signals that a file contains extended TFM data (OFM or JFM) rather than
plain TFM data."))

(defun load-font (file)
  "Load FILE into a new font, and return it.

Only actual TFM data is currently supported. If OFM or JFM data is detected,
this function signals an EXTENDED-TFM warning and returns NIL.

While loading TFM data, any signalled condition is restartable with
CANCEL-LOADING, in which case this function simply returns NIL."
  (with-open-file
      (*stream* file :direction :input :element-type '(unsigned-byte 8))
    (let ((lf (with-simple-restart (cancel-loading "Cancel loading this font.")
		(read-u16))))
      (cond ((zerop lf)
	     (warn 'extended-tfm :value "OFM" :file file))
	    ((or (= lf 9) (= lf 11))
	     (warn 'extended-tfm :value "JFM" :file file))
	    ((numberp lf)
	     (with-simple-restart (cancel-loading "Cancel loading this font.")
	       (load-tfm-font lf)))))))

;;; file.lisp ends here
