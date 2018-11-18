;;; tfm.lisp --- TeX Font Metrics

;; Copyright (C) 2018 Didier Verna

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

(in-package :cl-user)

(defpackage :net.didierverna.tfm
  (:documentation "The TeX Font Metrics package.")
  (:use :cl)
  (:export
    :nickname-package

    ;; Entry point:
    :parse))

(in-package :net.didierverna.tfm)


;; ==========================================================================
;; Utilities
;; ==========================================================================

;; --------
;; External
;; --------

(defun nickname-package (&optional (nickname :tfm))
  "Add NICKNAME (:TFM by default) to the :NET.DIDIERVERNA.TFM package."
  (rename-package :net.didierverna.tfm
		  (package-name :net.didierverna.tfm)
		  (adjoin nickname (package-nicknames :net.didierverna.tfm)
			  :test #'string-equal)))


;; --------
;; Internal
;; --------

(defun read-u16 (stream &optional limit)
  "Read an unsigned 16 bits Big Endian integer from STREAM.
If LIMIT, check that the integer is less than 2^15."
  (let ((u16 0))
    (setf (ldb (byte 8 8) u16) (read-byte stream)
	  (ldb (byte 8 0) u16) (read-byte stream))
    (when (and limit (not (zerop (ldb (byte 1 15) u16))))
      (error "Unsigned 16bits integer too large (>= 2^15): ~A." u16))
    u16))

(defun read-u32 (stream)
  "Read an unsigned 32 bits Big Endian integer from STREAM."
  (let ((u32 0))
    (setf (ldb (byte 8 24) u32) (read-byte stream)
	  (ldb (byte 8 16) u32) (read-byte stream)
	  (ldb (byte 8  8) u32) (read-byte stream)
	  (ldb (byte 8  0) u32) (read-byte stream))
    u32))

(defun read-fix (stream &optional limit)
  "Read a fix word from STREAM.
If LIMIT, check that the number lies within [-16,16]."
  (let* ((bytes (read-u32 stream))
	 (neg (= (ldb (byte 1 31) bytes) 1))
	 value)
    (when neg (setq bytes (lognot (1- bytes))))
    (setq value (+ (* (ldb (byte 8 24) bytes) (expt 2 4))
		   (* (ldb (byte 8 16) bytes) (expt 2 -4))
		   (* (ldb (byte 8  8) bytes) (expt 2 -12))
		   (* (ldb (byte 8  0) bytes) (expt 2 -20))))
    (when neg (setq value (- value)))
    ;; #### FIXME: <= or < ?
    (when (and limit (not (<= -16 value 16)))
      (error "Fix word outside [-16,16] range: ~A." value))
    value))



;; ==========================================================================
;; Non Trivial (Encoded) Character Information
;; ==========================================================================

;; #### NOTE: this section contains low level, intermediate data structures
;; that closely match the contents of TFM files. Because of that, we choose to
;; keep the TeX terminology.

;; ---------------------
;; Character information
;; ---------------------

(defstruct (char-info :conc-name)
  "The Char Info structure.
This structure is used to store decoded information from the char-info table
in TFM files."
  width-index height-index depth-index italic-index
  lig/kern-index next-char-code exten-index)

(defun decode-char-info (word)
  "Decode char-info WORD and return a CHAR-INFO instance."
  (let ((char-info (make-char-info
		    :width-index (ldb (byte 8 24) word)
		    :height-index (ldb (byte 4 20) word)
		    :depth-index (ldb (byte 4 16) word)
		    :italic-index (ldb (byte 6 10) word)))
	(tag (ldb (byte 2 8) word))
	(remainder (ldb (byte 8 0) word)))
    (case tag
      (1 (setf (lig/kern-index char-info) remainder))
      (2 (setf (next-char-code char-info) remainder))
      (3 (setf (exten-index char-info) remainder)))
    char-info))


;; ------------------
;; Extensible Recipes
;; ------------------

(defstruct (exten :conc-name)
  "The Exten structure.
This structure is used to store decoded information from the exten table in
TFM files."
  top mid bot rep)

(defun decode-exten (word)
  "Decode exten WORD and return an EXTEN instance."
  (make-exten
   :top (ldb (byte 8 24) word)
   :mid (ldb (byte 8 16) word)
   :bot (ldb (byte 8  8) word)
   :rep (ldb (byte 8  0) word)))



;; ==========================================================================
;; Character Metrics
;; ==========================================================================

;; -----
;; Class
;; -----

(defclass character-metrics ()
  ((code :initarg :code :reader code)
   (width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (depth :initarg :depth :reader depth)
   (italic-correction :initarg :italic-correction :reader italic-correction)
   (ligature/kerning-program :initform nil :accessor ligature/kerning-program)
   (next-larger-character :initform nil :accessor next-larger-character)
   (extension-recipe :initform nil :accessor extension-recipe))
  (:documentation "The Character Metrics class."))

(defmethod print-object ((character character-metrics) stream)
  "Print CHARACTER unreadably with its code to STREAM."
  (print-unreadable-object (character stream :type t)
    (princ (code character) stream)))

(defun make-character-metrics (code width height depth italic-correction)
  "Make a CHARACTER-METRICS instance."
  (make-instance 'character-metrics
    :code code
    :width width
    :height height
    :depth depth
    :italic-correction italic-correction))


;; ----------------
;; Pseudo-accessors
;; ----------------

;; #### NOTE: for now, extension recipes are stored as an array of top,
;; middle, bottom and repeated characters. I'm not sure whether storing
;; extension recipes within characters is the right way to do it (as opposed
;; to, for example, in the TFM instance directly). I guess I'll have to wait
;; until it's actually used to figure this out... The pseudo-accessors below
;; assume that an extension recipe exists in the target character.

(defun top-character (character)
  "Return the top character in CHARACTER's extension recipe, or NIL."
  (aref (extension-recipe character) 0))

(defun middle-character (character)
  "Return the middle character in CHARACTER's extension recipe, or NIL."
  (aref (extension-recipe character) 1))

(defun bottom-character (character)
  "Return the bottom character in CHARACTER's extension recipe, or NIL."
  (aref (extension-recipe character) 2))

(defun repeated-character (character)
  "Return the repeated character in CHARACTER's extension recipe."
  (aref (extension-recipe character) 3))



;; ==========================================================================
;; TeX Font Metrics
;; ==========================================================================

;; -----
;; Class
;; -----

(defclass tfm ()
  ((name :initarg :name :reader name)
   (checksum :accessor checksum)
   (design-size :accessor design-size)
   (slant :initform 0 :accessor slant)
   (interword-space :initform 0 :accessor interword-space)
   (interword-stretch :initform 0 :accessor interword-stretch)
   (interword-shrink :initform 0 :accessor interword-shrink)
   (ex :initform 0 :accessor ex)
   (em :initform 0 :accessor em)
   (extra-space :initform 0 :accessor extra-space)
   (min-code :accessor min-code)
   (max-code :accessor max-code)
   (characters-by-code :initform (make-hash-table :test #'eq)
		       :accessor characters-by-code)
   (characters :accessor characters))
  (:documentation "The TeX Font Metrics class."))

(defmethod print-object ((tfm tfm) stream)
  "Print TFM unreadably with its font name to STREAM."
  (print-unreadable-object (tfm stream :type t)
    (princ (name tfm) stream)))

(defun character-by-code (code tfm &optional errorp)
  "Return a character metrics object for CODE from TFM.
If ERRORP, signal an error if not found."
  (or (gethash code (characters-by-code tfm))
      (when errorp (error "Character code ~A not found." code))))

(defun (setf character-by-code) (character-metrics tfm)
  "Make CHARACTER-METRICS accessible by code in TFM."
  (setf (gethash (code character-metrics) (characters-by-code tfm))
	character-metrics))


;; ---------------------------
;; Ligature / Kerning Programs
;; ---------------------------

(defun skip-byte (instruction)
  "Return ligature / kerning INSTRUCTION's skip byte."
  (ldb (byte 8 24) instruction))

(defun next-byte (instruction)
  "Return ligature / kerning INSTRUCTION's next byte."
  (ldb (byte 8 16) instruction))

(defun op-byte (instruction)
  "Return ligature / kerning INSTRUCTION's op byte."
  (ldb (byte 8 8) instruction))

(defun remainder-byte (instruction)
  "Return ligature / kerning INSTRUCTION's remainder byte."
  (ldb (byte 8 0) instruction))

;; #### NOTE: the reason we return a list instead of multiple values is that
;; the caller of this function is a LOOP, which only knows how to destructure
;; lists.
(defun decode-ligature/kerning-instruction
    (instruction kernings characters &aux (skip-byte (skip-byte instruction)))
  "Decode a ligature / kerning INSTRUCTION.
The instruction may involve KERNINGS and CHARACTERS.
Return a list of multiple values, explained below.
The first value is the number of skips to reach the next step (0 if the
program should terminate). The second value, when present, is the decoded
instruction (none for halting)."
  (if (> skip-byte 128)
    (list 0)
    (let* ((stop (= skip-byte 128))
	   (skips (when (< skip-byte 128) skip-byte))
	   (next-char (aref characters (next-byte instruction)))
	   (op-byte (op-byte instruction))
	   (remainder (remainder-byte instruction))
	   (instruction
	     (if (>= op-byte 128)
	       (list next-char
		     :kern
		     (aref kernings (+ (* 256 (- op-byte 128)) remainder)))
	       (list next-char
		     :ligature (aref characters remainder)
		     :delete-before (when (member op-byte '(0 1 5)) t)
		     :delete-after (when (member op-byte '(0 2 6)) t)
		     :pass-over (cond ((member op-byte '(5 6 7)) 1)
				      ((= op-byte 11) 2)
				      (t 0))))))
      (list (if stop 0 skips) instruction))))

(defun %make-ligature/kerning-program (index instructions kernings characters)
  "Make a ligature / kerning program starting at INSTRUCTIONS[INDEX].
This program may involve KERNINGS and CHARACTERS."
  (loop :with continue := t
	:while continue
	:for (next instruction)
	  := (decode-ligature/kerning-instruction
	      (aref instructions index) kernings characters)
	:if instruction :collect instruction :else :collect :halt
	:if (zerop next)
	  :do (setq continue nil)
	:else
	  :do (incf index (1+ next))))

(defun make-ligature/kerning-program
    (index instructions kernings characters
     &aux (instruction (aref instructions index)))
  "Make a ligature / kerning program after finding its real start."
  (%make-ligature/kerning-program
   (if (> (skip-byte instruction) 128)
     (+ (* 256 (op-byte instruction)) (remainder-byte instruction))
     index)
   instructions kernings characters))



;; ==========================================================================
;; Parsing
;; ==========================================================================

;; ------
;; Header
;; ------

(defun parse-header (stream length tfm)
  "Parse a TFM header of LENGTH in STREAM."
  ;; #### WARNING: comparative tests with tftopl show a different checksum,
  ;; but the checksum I see doesn't fit 32 bits, so I don't understand what
  ;; they're doing...
  (setf (checksum tfm) (read-u32 stream))
  (setf (design-size tfm) (read-fix stream))
  (if (< (design-size tfm) 1)
    (error "Design size should be >= 1: ~A" (design-size tfm)))
  ;; #### NOTE: FILE-POSITION maybe?
  (loop :repeat (- length 2) :do (read-u32 stream)))


;; ---------------------
;; Character Information
;; ---------------------

(defun parse-character-information (stream nc nw nh nd ni nl nk ne tfm)
  "Parse the 8 TFM character information tables in STREAM."
  (let ((char-infos (make-array nc :fill-pointer 0))
	(widths (make-array nw :fill-pointer 0))
	(heights (make-array nh :fill-pointer 0))
	(depths (make-array nd :fill-pointer 0))
	(italics (make-array ni :fill-pointer 0))
	(lig/kerns (make-array nl :fill-pointer 0))
	(kerns (make-array nk :fill-pointer 0))
	(extens (make-array ne :fill-pointer 0)))
    (loop :repeat nc
	  :do (vector-push (decode-char-info (read-u32 stream)) char-infos))
    (loop :repeat nw :do (vector-push (read-fix stream t) widths))
    (loop :repeat nh :do (vector-push (read-fix stream t) heights))
    (loop :repeat nd :do (vector-push (read-fix stream t) depths))
    (loop :repeat ni :do (vector-push (read-fix stream t) italics))
    (loop :repeat nl :do (vector-push (read-u32 stream) lig/kerns))
    (loop :repeat nk :do (vector-push (read-fix stream t) kerns))
    (loop :repeat ne :do (vector-push (decode-exten (read-u32 stream)) extens))

    (loop :for array :in (list widths heights depths italics)
	  :for name :in (list "width" "height" "depth" "italic correction")
	  :unless (zerop (aref array 0))
	    :do (error "Invalid first element of ~A table (should be 0): ~A."
		       name (aref array 0)))

    ;; 1. Create the character metrics.
    (loop :for char-info :across char-infos
	  :for code :from (min-code tfm)
	  :unless (zerop (width-index info))
	    :do (setf (character-by-code tfm)
		      (make-character-metrics
		       code
		       (aref widths (width-index char-info))
		       (aref heights (height-index char-info))
		       (aref depths (depth-index char-info))
		       (aref italics (italic-index char-info)))))
    (setf (characters tfm) (hash-table-count (characters-by-code tfm)))

    ;; Now that we have all the characters registered, we can start processing
    ;; mutual references.

    ;; 2. Create extension recipes.
    (loop :for char-info :across char-infos
	  :for code :from (min-code tfm)
	  :when (exten-index char-info)
	    :do (let ((exten (aref extens (exten-index char-info)))
		      (recipe (make-array 4 :initial-element nil)))
		  (loop :for code :in (list (top exten) (mid exten) (bot exten))
			:for index :from 0
			:unless (zerop code)
			  :do (setf (aref recipe index)
				    (character-by-code code tfm t)))
		  (setf (aref recipe 3)
			(character-by-code (rep exten) tfm t))
		  (setf (extension-recipe (character-by-code code tfm t))
			recipe)))

    #+()(loop :for character :across (characters tfm)
	      :do (setf (width character) (aref widths (width character))
			(height character) (aref heights (height character))
			(depth character) (aref depths (depth character))
			(italic-correction character) (aref italic-corrections
							    (italic-correction
							     character)))
	      :when (next-larger-character character)
		:do (setf (next-larger-character character)
			  (aref (characters tfm) (next-larger-character
						  character)))
	      :when (ligature/kerning-program character)
		:do (setf (ligature/kerning-program character)
			  (make-ligature/kerning-program
			   (ligature/kerning-program character)
			   ligatures/kernings
			   kernings
			   (characters tfm)))
	      )))


;; ----------
;; Parameters
;; ----------

(defun parse-parameters (stream length tfm)
  "Parse a TFM parameters section in STREAM."
  (when (>= length 1) (setf (slant tfm) (read-fix stream)))
  (when (>= length 2) (setf (interword-space tfm) (read-fix stream t)))
  (when (>= length 3) (setf (interword-stretch tfm) (read-fix stream t)))
  (when (>= length 4) (setf (interword-shrink tfm) (read-fix stream t)))
  (when (>= length 5) (setf (ex tfm) (read-fix stream t)))
  (when (>= length 6) (setf (em tfm) (read-fix stream t)))
  (when (>= length 7) (setf (extra-space tfm) (read-fix stream t))))



;; ==========================================================================
;; Entry Point
;; ==========================================================================

(defun parse (file &aux (tfm (make-instance 'tfm :name (pathname-name file))))
  "Parse TFM FILE into a TFM instance. Return that instance."
  (with-open-file (stream file
		   :direction :input :element-type '(unsigned-byte 8))
    ;; 1. Read the preamble and perform some sanity checks.
    (let ((lf (read-u16 stream t))
	  (lh (read-u16 stream t))
	  (bc (read-u16 stream t))
	  (ec (read-u16 stream t))
	  (nw (read-u16 stream t))
	  (nh (read-u16 stream t))
	  (nd (read-u16 stream t))
	  (ni (read-u16 stream t))
	  (nl (read-u16 stream t))
	  (nk (read-u16 stream t))
	  (ne (read-u16 stream t))
	  (np (read-u16 stream t))
	  nc)
      (unless (and (<= (1- bc) ec) (<= ec 255))
	(error "Invalid smallest / largest character codes: ~A / ~A." bc ec))
      (when (> bc 255) (setq bc 1 ec 0))
      (setq nc (+ ec (- bc) 1))
      (setf (min-code tfm) bc (max-code tfm) ec)
      (unless (= lf (+ 6 lh nc nw nh nd ni nl nk ne np))
	(error "Declared section lengths mismatch."))
      (let ((actual-file-length (file-length stream))
	    (declared-file-length (* 4 lf)))
	(unless (= actual-file-length declared-file-length)
	  (error "Actual / declared file lengths mismatch: ~A / ~A."
		 actual-file-length declared-file-length)))
      (loop :for length :in (list nw nh nd ni ne)
	    :for min :in '(1 1 1 1 0)
	    :for max :in '(256 16 16 64 256)
	    :for name :in '("width" "height" "depth" "italic correction"
			    "exten")
	    :unless (<= min length max)
	      :do (error "Invalid ~A table length (out of range): ~A."
			 name length))
      (unless (>= lh 2) (error "Invalid header length (too small): ~A." lh))
      ;; 2. Read the header section.
      (parse-header stream lh tfm)
      ;; 3. Read the 8 character-related sections.
      (parse-character-information stream nc nw nh nd ni nl nk ne tfm)
      ;; 4. Read the parameters section.
      (parse-parameters stream np tfm)))
  tfm)

;; tfm.lisp ends here
