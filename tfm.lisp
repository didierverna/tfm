;;; tfm.lisp --- TeX Font Metrics

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

(in-package :cl-user)

(defpackage :net.didierverna.tfm
  (:documentation "The TeX Font Metrics package.")
  (:use :cl)
  (:export
    ;; Utilities:
    :nickname-package
    ;; Character metrics:
    :code :width :height :depth :italic-correction :next-larger-character
    :extension-recipe :top-character :middle-character :bottom-character
    :repeated-character
    ;; TeX font metrics:
    :name :checksum :design-size :slant
    :interword-space :interword-stretch :interword-shrink :ex :em :extra-space
    :min-code :max-code
    :character-by-code :character-count
    :ligature :kerning :right-boundary-character
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
      (error "Unsigned 16 bits integer too large (>= 2^15): ~A." u16))
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
;; Non Trivial (Encoded) Information
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
in TFM files (see TeX: the Program [543]). Only one of LIG/KERN-INDEX,
NEXT-CHAR, and EXTEN-INDEX may be non-null at a time (see TeX: the Program
[544])."
  width-index height-index depth-index italic-index
  lig/kern-index next-char exten-index)

(defun decode-char-info (word)
  "Decode char-info WORD into a new CHAR-INFO instance, and return it."
  (let ((char-info (make-char-info
		    :width-index (ldb (byte 8 24) word)
		    :height-index (ldb (byte 4 20) word)
		    :depth-index (ldb (byte 4 16) word)
		    :italic-index (ldb (byte 6 10) word)))
	(tag (ldb (byte 2 8) word))
	(remainder (ldb (byte 8 0) word)))
    (case tag
      (1 (setf (lig/kern-index char-info) remainder))
      (2 (setf (next-char char-info) remainder))
      (3 (setf (exten-index char-info) remainder)))
    char-info))


;; ------------------
;; Extensible Recipes
;; ------------------

(defstruct (exten :conc-name)
  "The Exten structure.
This structure is used to store decoded information from the exten table in
TFM files (see TeX: the Program [546])."
  top mid bot rep)

(defun decode-exten (word)
  "Decode exten WORD into a new EXTEN instance, and return it."
  (make-exten
   :top (ldb (byte 8 24) word)
   :mid (ldb (byte 8 16) word)
   :bot (ldb (byte 8  8) word)
   :rep (ldb (byte 8  0) word)))


;; -------------------------------
;; Ligature / Kerning Instructions
;; -------------------------------

(defstruct (lig/kern :conc-name)
  "The Lig/Kern structure.
This structure is used to store decoded information from the lig/kern table in
TFM files (see TeX: the Program [545])."
  skip next op remainder)

(defun decode-lig/kern (word)
  "Decode lig/kern WORD into a new LIG/KERN instance, and return it."
  (make-lig/kern
   :skip (ldb (byte 8 24) word)
   :next (ldb (byte 8 16) word)
   :op (ldb (byte 8 8) word)
   :remainder (ldb (byte 8 0) word)))



;; ==========================================================================
;; Character Metrics
;; ==========================================================================

;; -----
;; Class
;; -----

(defclass character-metrics ()
  ((code
    :documentation "The character's numerical code."
    :initarg :code
    :reader code)
   (width
    :documentation "The character's width, in design size units."
    :initarg :width
    :reader width)
   (height
    :documentation "The character's height, in design size units."
    :initarg :height
    :reader height)
   (depth
    :documentation "The character's depth, in design size units."
    :initarg :depth
    :reader depth)
   (italic-correction
    :documentation "The character's italic correction, in design size units."
    :initarg :italic-correction
    :reader italic-correction)
   ;; #### WARNING: for now, the handling of character lists is very basic: I
   ;; only store the "next character" here, exactly as it is provided in the
   ;; TFM file. There's no specific representation of character lists and no
   ;; checking for cycles for instance. I'm not sure whether storing the next
   ;; character here is the right way to do it (as opposed to creating
   ;; character lists in the TFM instance directly). I guess I'll have to wait
   ;; until it's actually used to figure this out.
   (next-larger-character
    :documentation "The character's next larger character.
This slot is non-null only if the character is part of a chain of characters
of ascending size, and not the last one (see TeX: the Program [544]). It is
mutually exclusive with the EXTENSION-RECIPE slot, and also with the existence
of a ligature or kerning program for this character."
    :initform nil
    :accessor next-larger-character)
   ;; #### WARNING: for now, extension recipes are stored as an array of top,
   ;; middle, bottom and repeated characters. I'm not sure whether storing
   ;; extension recipes within characters is the right way to do it (as
   ;; opposed to, for example, in the TFM instance directly). I guess I'll
   ;; have to wait until it's actually used to figure this out.
   (extension-recipe
    :documentation "The character's extension recipe.
This slot is non-null only if this character is extensible (see TeX: the
Program [544]). It is mutually exclusive with the NEXT-LARGER-CHARACTER slot,
and also with the existence of a ligature or kerning program for this
character."
    :initform nil
    :accessor extension-recipe))
  (:documentation "The Character Metrics class.
This class represents decoded character information from the TFM format.
Within the context of this library, the term \"character\" denotes an instance
of this class."))

(defmethod print-object ((character character-metrics) stream)
  "Print CHARACTER unreadably with its code to STREAM."
  (print-unreadable-object (character stream :type t)
    (princ (code character) stream)))

(defun make-character-metrics (code width height depth italic-correction)
  "Make a new CHARACTER-METRICS instance, and return it.
Initialize the character's CODE, WIDTH, HEIGHT, DEPTH, and ITALIC-CORRECTION
appropriately. The two remaining slots (NEXT-LARGER-CHARACTER and
EXTENSION-RECIPE) will be initialized later if needed, when all character
metrics instances are created."
  (make-instance 'character-metrics
    :code code
    :width width
    :height height
    :depth depth
    :italic-correction italic-correction))


;; ----------------
;; Pseudo-accessors
;; ----------------

;; #### NOTE: The pseudo-accessors below assume that an extension recipe
;; exists in the target character.

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
  ((name
    :documentation "The font's name. This is the TFM file's base name."
    :initarg :name
    :reader name)
   (checksum :documentation "The TFM file's checksum." :accessor checksum)
   (design-size
    :documentation "The font's design size, in units of TeX points."
    :accessor design-size)
   (slant :documentation "The font's slant ratio." :initform 0 :accessor slant)
   (interword-space
    :documentation "The font's normal interword space, in design size units."
    :initform 0
    :accessor interword-space)
   (interword-stretch
    :documentation "The font's interword stretchability, in design size units."
    :initform 0
    :accessor interword-stretch)
   (interword-shrink
    :documentation "The font's interword shrinkability, in design size units."
    :initform 0
    :accessor interword-shrink)
   (ex :documentation "The font's ex size." :initform 0 :accessor ex)
   (em  :documentation "The font's em size." :initform 0 :accessor em)
   (extra-space
    :documentation "The font's extra space to put at the end of sentences."
    :initform 0 :accessor extra-space)
   (min-code
    :documentation "The font's smallest character code."
    :accessor min-code)
   (max-code
    :documentation "The font's largest character code."
    :accessor max-code)
   (characters
    :documentation "The font's characters.
This is a hash table associating character codes with characters."
    :initform (make-hash-table :test #'eq)
    :accessor characters)
   (character-count
    :documentation "The font's number of characters."
    :accessor character-count)
   (ligatures
    :documentation "The font's ligatures.
This is a hash table associating conses of characters with the corresponding
ligature."
    :initform (make-hash-table :test #'equal)
    :accessor ligatures)
   (kernings
    :documentation "The font's kernings.
This is a hash table associating conses of characters with the corresponding
kerning, in design size units."
    :initform (make-hash-table :test #'equal)
    :accessor kernings)
   (right-boundary-character
    :documentation "The font's right boundary character.
This is either a character from this font, or a character code outside of this
font's code boundaries (see TeX: the Program [545])."
    :initform nil
    :accessor right-boundary-character))
  (:documentation "The TeX Font Metrics class.
This class represents decoded font information from the TFM format. Within the
context of this library, the term \"tfm\" denotes an instance of this class."))

(defmethod print-object ((tfm tfm) stream)
  "Print TFM unreadably with its font name to STREAM."
  (print-unreadable-object (tfm stream :type t)
    (princ (name tfm) stream)))

(defun make-tfm (name)
  "Make a new TFM instance, and return it.
Only font NAME is initialized. The other slots will be computed later on."
  (make-instance 'tfm :name name))


;; ----------------
;; Pseudo-accessors
;; ----------------

(defun character-by-code (code tfm &optional errorp)
  "Return TFM's character with CODE.
If ERRORP, signal an error if not found."
  (or (gethash code (characters tfm))
      (when errorp (error "Character code ~A not found in ~S." code tfm))))

(defun (setf character-by-code) (character tfm)
  "Make TFM's CHARACTER accessible by its code."
  (setf (gethash (code character) (characters tfm)) character))

(defun ligature (character1 character2 tfm)
  "Return TFM's ligature for CHARACTER1 and CHARACTER2 if any."
  (gethash (cons character1 character2) (ligatures tfm)))

(defun (setf ligature) (ligature character1 character2 tfm)
  "Set TFM's LIGATURE for CHARACTER1 and CHARACTER2."
  (setf (gethash (cons character1 character2) (ligatures tfm)) ligature))

(defun kerning (character1 character2 tfm)
  "Return TFM's kerning for CHARACTER1 and CHARACTER2 if any."
  (gethash (cons character1 character2) (kernings tfm)))

(defun (setf kerning) (kerning character1 character2 tfm)
  "Set TFM's KERNING for CHARACTER1 and CHARACTER2."
  (setf (gethash (cons character1 character2) (kernings tfm)) kerning))



;; ==========================================================================
;; Parsing
;; ==========================================================================

;; ------
;; Header
;; ------

(defun parse-header (stream length tfm)
  "Parse a header of LENGTH from STREAM into TFM."
  ;; #### FIXME: comparative tests with tftopl show a different checksum, but
  ;; the checksum I see doesn't fit 32 bits, so I don't understand what
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

;; #### FIXME: Knuth's description of the lig/kern programming language is
;; somewhat confusing. I don't understand the purpose of "halt" (skip > 128)
;; in particular. What's the difference with having no instruction associated
;; with a character at all? Otherwise, it would be in contradiction with the
;; description of regular instruction (perform and then stop).

(defclass ligature ()
  ((composite
    :documentation "The character to insert."
    :initarg :composite
    :reader composite)
   (delete-before
    :documentation "Whether to delete the character before the ligature."
    :initarg :delete-before
    :reader delete-before)
   (delete-after
    :documentation "Whether to delete the character after the ligature."
    :initarg :delete-after
    :reader delete-after)
   (pass-over
    :documentation
    "The number of characters to skip for reaching the next character."
    :initarg :pass-over
    :reader pass-over))
  (:documentation "The Ligature class.
This class represents a decoded ligature program from the TFM format. Within
the context of this library, the term \"ligature\" denotes an instance of this
class."))

(defun make-ligature (composite delete-before delete-after pass-over)
  "Make a new LIGATURE instance, and return it."
  (make-instance 'ligature
    :composite composite
    :delete-before delete-before
    :delete-after delete-after
    :pass-over pass-over))

(defun %make-ligature/kerning-program (character index lig/kerns kerns tfm)
  "Make a ligature/kerning program for CHARACTER in TFM.
The program starts at LIG/KERNS[INDEX] and uses the KERNS array."
  (loop :with continue := t
	:while continue
	:for lig/kern := (aref lig/kerns index)
	:when (<= (skip lig/kern) 128)
	  :if (>= (op lig/kern) 128)
	    :do (setf (kerning character
			       (character-by-code (next lig/kern) tfm t)
			       tfm)
		      (aref kerns (+ (* 256 (- (op lig/kern) 128))
				     (remainder lig/kern))))
	  :else
	    :do (setf (ligature character
				(character-by-code (next lig/kern) tfm t)
				tfm)
		      (make-ligature
		       (character-by-code (remainder lig/kern) tfm t)
		       (when (member (op lig/kern) '(0 1 5)) t)
		       (when (member (op lig/kern) '(0 2 6)) t)
		       (cond ((member (op lig/kern) '(5 6 7)) 1)
			     ((= (op lig/kern) 11) 2)
			     (t 0))))
	:if (>= (skip lig/kern) 128)
	  :do (setq continue nil)
	:else
	  :do (incf index (1+ (skip lig/kern)))))

(defun make-ligature/kerning-program
    (character index lig/kerns kerns tfm &aux (lig/kern (aref lig/kerns index)))
  "Find the real start of a ligature/kerning program and make it.
See %make-ligature/kerning-program for more information."
  (%make-ligature/kerning-program
   character
   (if (> (skip lig/kern) 128)
     (+ (* 256 (op lig/kern)) (remainder lig/kern))
     index)
   lig/kerns
   kerns
   tfm))


(defun make-extension-recipe (exten tfm)
  "Make an extension recipe based on EXTEN and TFM."
  (let ((recipe (make-array 4 :initial-element nil)))
    (loop :for code :in (list (top exten) (mid exten) (bot exten))
	  :for index :from 0
	  :unless (zerop code)
	    :do (setf (aref recipe index) (character-by-code code tfm t)))
    (setf (aref recipe 3) (character-by-code (rep exten) tfm t))
    recipe))


(defun parse-character-information (stream nc nw nh nd ni nl nk ne tfm)
  "Parse the 8 TFM character information tables from STREAM into TFM."
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
	  :do (vector-push (decode-char-info (read-u32 stream)) char-infos))
    (loop :repeat nw :do (vector-push (read-fix stream t) widths))
    (loop :repeat nh :do (vector-push (read-fix stream t) heights))
    (loop :repeat nd :do (vector-push (read-fix stream t) depths))
    (loop :repeat ni :do (vector-push (read-fix stream t) italics))
    (loop :repeat nl
	  :do (vector-push (decode-lig/kern (read-u32 stream)) lig/kerns))
    (loop :repeat nk :do (vector-push (read-fix stream t) kerns))
    (loop :repeat ne :do (vector-push (decode-exten (read-u32 stream)) extens))

    (loop :for array :in (list widths heights depths italics)
	  :for name :in (list "width" "height" "depth" "italic correction")
	  :unless (zerop (aref array 0))
	    :do (error "Invalid first element of ~A table (should be 0): ~A."
		       name (aref array 0)))

    ;; 2. Check for left and right boundary characters.
    ;; #### WARNING: the left and right boundary characters thing is still
    ;; unclear to me (e.g. why a right boundary but a left lig/kern program?).
    ;; I still need to see this used to figure out which implementation is
    ;; best. Right now, the right boundary character is stored in the TFM
    ;; instance directly, whereas there is a special lig/kern program
    ;; accessible from :LEFT-BOUNDARY-CHARACTER which is probably not very
    ;; good.
    (unless (zerop nl)
      (let ((lig/kern (aref lig/kerns 0)))
	(when (= (skip lig/kern) 255)
	  (setf (right-boundary-character tfm)
		(or (character-by-code (next lig/kern) tfm)
		    (next lig/kern)))))
      ;; #### NOTE: there would be a problem for lig/kern arrays of size 1
      ;; since the first element would also be the last one. I don't think
      ;; this could happen however, as it would mean that only boundary
      ;; characters would have lig/kern instructions, not regular ones.
      (let ((lig/kern (aref lig/kerns (1- (length lig/kerns)))))
	(when (= (skip lig/kern) 255)
	  ;; #### NOTE: since we need to access the last instruction in the
	  ;; lig/kern table, we may as well bypass
	  ;; MAKE-LIGATURE/KERNING-PROGRAM.
	  (%make-ligature/kerning-program
	   :left-boundary-character
	   (+ (* 256 (op lig/kern)) (remainder lig/kern))
	   lig/kerns
	   kerns
	   tfm))))

    ;; 3. Create the character metrics.
    (loop :for char-info :across char-infos
	  :for code :from (min-code tfm)
	  :unless (zerop (width-index char-info))
	    :do (setf (character-by-code tfm)
		      (make-character-metrics
		       code
		       (aref widths (width-index char-info))
		       (aref heights (height-index char-info))
		       (aref depths (depth-index char-info))
		       (aref italics (italic-index char-info)))))
    ;; #### NOTE: this should in fact always be ec - bc + 1.
    (setf (character-count tfm) (hash-table-count (characters tfm)))

    ;; 4. Now that we have all the characters registered, we can start
    ;; processing mutual references: character lists, extension recipes,
    ;; ligature, and kerning instructions.
    (loop :for char-info :across char-infos
	  :for code :from (min-code tfm)
	  :when (lig/kern-index char-info)
	    :do (make-ligature/kerning-program
		 (character-by-code code tfm t)
		 (lig/kern-index char-info)
		 lig/kerns
		 kerns
		 tfm)
	  :when (next-char char-info)
	    :do (setf (next-larger-character (character-by-code code tfm t))
		      (character-by-code (next-char char-info) tfm t))
	  :when (exten-index char-info)
	    :do (setf (extension-recipe (character-by-code code tfm t))
		      (make-extension-recipe
		       (aref extens (exten-index char-info))
		       tfm)))))


;; ----------
;; Parameters
;; ----------

(defun parse-parameters (stream length tfm)
  "Parse a parameters section of LENGTH from STREAM into TFM."
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

(defun parse (file &aux (tfm (make-tfm (pathname-name file))))
  "Parse TFM FILE into a new TFM instance, and return it."
  (with-open-file
      (stream file :direction :input :element-type '(unsigned-byte 8))
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
      ;; 2. Parse the header section.
      (parse-header stream lh tfm)
      ;; 3. Parse the 8 character-related sections.
      (parse-character-information stream nc nw nh nd ni nl nk ne tfm)
      ;; 4. Parse the parameters section.
      (parse-parameters stream np tfm)))
  tfm)

;; tfm.lisp ends here
