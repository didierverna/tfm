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

;; #### FIXME: check all calls to character-by-code ... T to see whether it's
;; #### actually required that the character exist in the font.


;;; Code:

(in-package :net.didierverna.tfm)
(in-readtable :net.didierverna.tfm)


;; ==========================================================================
;; Header
;; ==========================================================================

(define-condition invalid-design-size (tfm-compliance-error)
  ((value :initarg :value :accessor value))
  (:report (lambda (invalid-design-size stream)
	     (stream-report stream invalid-design-size
	       "~Apt~:P design size is too small (< 1pt)."
	       (value invalid-design-size))))
  (:documentation "The Invalid Design Size error.
It signals that a design size VALUE is too small (< 1pt)."))

(defun parse-header (stream length font)
  "Parse a header of LENGTH words from STREAM into FONT."
  (setf (checksum font) (read-u32 stream))
  (setf (design-size font) (read-fix-word stream))
  (unless (>= (design-size font) 1)
    (restart-case (error 'invalid-design-size
		    :value (design-size font) :stream stream)
      (use-one () :report "Set to 1pt." (setf (design-size font) 1))))
  (decf length 2)
  ;; #### WARNING: we silently assume Xerox PARC headers below. Not sure if
  ;; anything else could be in use, but it's impossible to tell from the files
  ;; themselves.
  (when (>= length 10)
    (setf (encoding font) (read-padded-string stream 40))
    (decf length 10))
  (when (>= length 5)
    (setf (family font) (read-padded-string stream 20))
    (decf length 5))
  (when (>= length 1)
    (let ((word (read-u32 stream)))
      (setf (7bits-safe font) (ldb (byte 1 31) word))
      (setf (face font) (ldb (byte 8 0) word)))
    (decf length))
  (loop :repeat length :do (read-u32 stream)))



;; ==========================================================================
;; Character Information Tables
;; ==========================================================================

;; -------------------------
;; Ligature/Kerning Programs
;; -------------------------

(defun %make-ligature/kerning-program (character index lig/kerns kerns font)
  "Make a ligature/kerning program for CHARACTER in FONT.
The program starts at LIG/KERNS[INDEX] and uses the KERNS array."
  (loop :with continue := t
	:while continue
	:for lig/kern := (aref lig/kerns index)
	:when (<= (skip lig/kern) 128)
	  :if (>= (op lig/kern) 128)
	    :do (setf (kerning character
			       (character-by-code (next lig/kern) font t)
			       font)
		      (aref kerns (+ (* 256 (- (op lig/kern) 128))
				     (remainder lig/kern))))
	  :else
	    :do (setf (ligature character
				(character-by-code (next lig/kern) font t)
				font)
		      (make-ligature
		       (character-by-code (remainder lig/kern) font t)
		       (when (member (op lig/kern) '(0 1 5)) t)
		       (when (member (op lig/kern) '(0 2 6)) t)
		       (cond ((member (op lig/kern) '(5 6 7)) 1)
			     ((= (op lig/kern) 11) 2)
			     (t 0))))
	:if (>= (skip lig/kern) 128)
	  :do (setq continue nil)
	:else
	  :do (incf index (1+ (skip lig/kern)))))

(defun make-ligature/kerning-program (character index lig/kerns kerns font
				      &aux (lig/kern (aref lig/kerns index)))
  "Find the real start of a ligature/kerning program and make it.
See %make-ligature/kerning-program for more information."
  (%make-ligature/kerning-program
   character
   (if (> (skip lig/kern) 128)
     (+ (* 256 (op lig/kern)) (remainder lig/kern))
     index)
   lig/kerns
   kerns
   font))


;; -----------------
;; Extension Recipes
;; -----------------

(defun make-extension-recipe (exten font)
  "Make an extension recipe based on EXTEN and FONT."
  (let ((recipe (make-array 4 :initial-element nil)))
    (loop :for code :in (list (top exten) (mid exten) (bot exten))
	  :for index :from 0
	  :unless (zerop code)
	    :do (setf (aref recipe index) (character-by-code code font t)))
    (setf (aref recipe 3) (character-by-code (rep exten) font t))
    recipe))


;; ---------------------
;; Character Information
;; ---------------------

(define-condition invalid-char-info (tfm-compliance-error)
  ((char-info :initarg :char-info :accessor char-info))
  (:report (lambda (invalid-char-info stream)
	     (stream-report stream invalid-char-info
	       "char-info ~A is invalid (should be 0 0 0 0 NIL NIL NIL)."
	       (char-info invalid-char-info))))
  (:documentation "The Invalid Char Info error.
It signals that a CHAR-INFO with a width-index of 0 is not completely zero'ed
out."))

(define-condition tfm-table-error (tfm-compliance-error)
  ((name :initarg :name :accessor name))
  (:documentation "The TFM table errors root condition.
This is the root condition for errors related to a NAMEd TFM table."))

(define-condition invalid-table-start (tfm-table-error)
  ((value :initarg :value :accessor value))
  (:report (lambda (invalid-table-start stream)
	     (stream-report stream invalid-table-start
	       "first value ~A in ~A table is invalid (should be 0)."
	       (value invalid-table-start)
	       (name invalid-table-start))))
  (:documentation "The Invalid Table Start error.
It signals that the first VALUE in a table is not 0."))

(defun parse-character-information (stream nc nw nh nd ni nl nk ne font)
  "Parse the 8 character information tables from STREAM into FONT."
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
	  :for word = (read-u32 stream)
	  :for char-info := (decode-char-info word)
	  :unless (or (not (zerop (width-index char-info))) (zerop word))
	    :do (restart-case (error 'invalid-char-info
				:char-info char-info :stream stream)
		  (use-zero () :report "Zero it out."
		    (setq char-info (decode-char-info 0))))
	  :do (vector-push char-info char-infos))
    (loop :for name :in (list "width" "height" "depth" "italic correction")
	  :for array :in (list widths heights depths italics)
	  :for length :in (list nw nh nd ni)
	  :do (vector-push (read-fix-word stream t) array)
	  :unless (zerop (aref array 0))
	    :do (restart-case
		    (error 'invalid-table-start
		      :value (aref array 0) :name name :stream stream)
		  (use-zero () :report "Set to 0." (setf (aref array 0) 0)))
	  :do (loop :repeat (1- length)
		    :do (vector-push (read-fix-word stream t) array)))
    (loop :repeat nl
	  :do (vector-push (decode-lig/kern (read-u32 stream)) lig/kerns))
    (loop :repeat nk :do (vector-push (read-fix-word stream t) kerns))
    (loop :repeat ne :do (vector-push (decode-exten (read-u32 stream)) extens))

    ;; 2. Create the character metrics.
    (loop :for char-info :across char-infos
	  :for code :from (min-code font)
	  :unless (zerop (width-index char-info))
	    :do (setf (character-by-code font)
		      (make-character-metrics
		       code
		       (aref widths (width-index char-info))
		       (aref heights (height-index char-info))
		       (aref depths (depth-index char-info))
		       (aref italics (italic-index char-info)))))
    ;; #### NOTE: this count doesn't include a right boundary character
    ;; potentially added below.
    (setf (character-count font) (hash-table-count (characters font)))

    ;; Now that we have all the characters registered, we can start processing
    ;; mutual references.

    ;; 3. Check for right and left boundary characters.
    ;; #### WARNING: the left and right boundary characters thing is still
    ;; unclear to me (e.g. why a right boundary but a left lig/kern program?).
    ;; I still need to see this used to figure out which implementation is
    ;; best. Right now, the right boundary character is stored in the FONT
    ;; instance directly, whereas there is a special lig/kern program
    ;; accessible from :LEFT-BOUNDARY-CHARACTER which is probably not very
    ;; good.
    ;; #### FIXME: what happens when nl = 1 and the first lig/kern instruction
    ;; is also the last one?
    ;; #### FIXME: can a right boundary character be a normal one, with
    ;; non-zero metrics?
    (unless (zerop nl)
      (let ((lig/kern (aref lig/kerns 0)))
	(when (= (skip lig/kern) 255)
	  (let ((code (next lig/kern)))
	    (setf (right-boundary-character font)
		  (or (character-by-code code font)
		      (setf (character-by-code font)
			    (make-character-metrics code 0 0 0 0)))))))
      (let ((lig/kern (aref lig/kerns (1- nl))))
	(when (= (skip lig/kern) 255)
	  ;; #### NOTE: since we need to access the last instruction in the
	  ;; lig/kern table, we may as well bypass
	  ;; MAKE-LIGATURE/KERNING-PROGRAM.
	  (%make-ligature/kerning-program
	   :left-boundary-character
	   (+ (* 256 (op lig/kern)) (remainder lig/kern))
	   lig/kerns
	   kerns
	   font))))

    ;; 4. Process ligature / kerning programs, character lists, and extension
    ;; recipes, character by character.
    (loop :for char-info :across char-infos
	  :for code :from (min-code font)
	  :when (lig/kern-index char-info)
	    :do (make-ligature/kerning-program
		 (character-by-code code font t)
		 (lig/kern-index char-info)
		 lig/kerns
		 kerns
		 font)
	  :when (next-char char-info)
	    :do (setf (next-larger-character (character-by-code code font t))
		      (character-by-code (next-char char-info) font t))
	  :when (exten-index char-info)
	    :do (setf (extension-recipe (character-by-code code font t))
		      (make-extension-recipe
		       (aref extens (exten-index char-info))
		       font)))))



;; ==========================================================================
;; Parameters Section
;; ==========================================================================

(defun parse-parameters (stream length font)
  "Parse a parameters section of LENGTH words from STREAM into FONT."
  (when (>= length 1) (setf (slant font) (read-fix-word stream)))
  (when (>= length 2) (setf (interword-space font) (read-fix-word stream t)))
  (when (>= length 3) (setf (interword-stretch font) (read-fix-word stream t)))
  (when (>= length 4) (setf (interword-shrink font) (read-fix-word stream t)))
  (when (>= length 5) (setf (ex font) (read-fix-word stream t)))
  (when (>= length 6) (setf (em font) (read-fix-word stream t)))
  ;; #### FIXME: in mathsy fonts, there is no extra-space, but 16 additional
  ;; parameters instead. In mathex fonts, the extra-space is here, and there
  ;; is 6 additional parameters. We need to figure this out by looking at the
  ;; font encoding. It is likely that the best solution would be to have
  ;; a font class hierarchy instead of a single one.
  (when (>= length 7) (setf (extra-space font) (read-fix-word stream t))))



;; ==========================================================================
;; Preamble
;; ==========================================================================

(define-condition file-size-mixin ()
  ((declared-size :initarg :declared-size :accessor declared-size)
   (actual-size :initarg :actual-size :accessor actual-size))
  (:documentation "The File Size Mixin condition.
It is used in both errors and warnings to report different DECLARED- and
ACTUAL-SIZEs."))

(define-condition file-underflow (file-size-mixin tfm-compliance-error)
  ()
  (:report (lambda (file-underflow stream)
	     (stream-report stream file-underflow
	       "actual file size ~A is lesser than declared one ~A."
	       (actual-size file-underflow)
	       (declared-size file-underflow))))
  (:documentation "The File Underflow error.
It signals that the file size is shorter than expected."))

;; #### NOTE: this one is a warning instead of an error because TeX silently
;; ignores junk at the end of TFM files (see TeX: the Program [575]). We hence
;; do the same, but still signal a warning.
(define-condition file-overflow (file-size-mixin tfm-compliance-warning)
  ()
  (:report (lambda (file-overflow stream)
	     (stream-report stream file-overflow
	       "declared file size ~A is lesser than actual one ~A."
	       (declared-size file-overflow)
	       (actual-size file-overflow))))
  (:documentation "The File Overflow warning.
It signals that the file size is longer than expected."))

(define-condition invalid-header-length (tfm-compliance-error)
  ((value :initarg :value :accessor value))
  (:report (lambda (invalid-header-length stream)
	     (stream-report stream invalid-header-length
	       "~A word~:P header length is too small (< 2 words)."
	       (value invalid-header-length))))
  (:documentation "The Invalid Header Length error.
It signals that a header length VALUE is too small (< 2 words)."))

(define-condition invalid-character-range (tfm-compliance-error)
  ((bc :initarg :bc :accessor bc)
   (ec :initarg :ec :accessor ec))
  (:report (lambda (invalid-character-range stream)
	     (stream-report stream invalid-character-range
	       "~
character range ~A (bc) - ~A (ec) doesn't satisfy bc-1 <= ec && ec <= 255)."
	       (bc invalid-character-range)
	       (ec invalid-character-range))))
  (:documentation "The Invalid Character Range error.
It signals that BC-1 > EC, or that EC > 255."))

(define-condition invalid-section-lengths (tfm-compliance-error)
  ((lf :initarg :lf :accessor lf)
   (lh :initarg :lh :accessor lh)
   (nc :initarg :nc :accessor nc)
   (nw :initarg :nw :accessor nw)
   (nh :initarg :nh :accessor nh)
   (nd :initarg :nd :accessor nd)
   (ni :initarg :ni :accessor ni)
   (nl :initarg :nl :accessor nl)
   (nk :initarg :nk :accessor nk)
   (ne :initarg :ne :accessor ne)
   (np :initarg :np :accessor np))
  (:report (lambda (section-lengths stream)
	     (stream-report stream section-lengths
	       "~
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
  (:documentation "The Section Lengths error.
It signals that LF != 6 + LH + NC + NW + NH + ND + NI + NL + NK + NE + NP."))

(define-condition invalid-table-length (tfm-table-error)
  ((smallest :initarg :smallest :accessor smallest)
   (largest :initarg :largest :accessor largest)
   (value :initarg :value :accessor value))
  (:report (lambda (invalid-table-length stream)
	     (stream-report stream invalid-table-length
	       "~
~A table length ~A is invalid (should be in [~A,~A])."
	       (name invalid-table-length)
	       (value invalid-table-length)
	       (smallest invalid-table-length)
	       (largest invalid-table-length))))
  (:documentation "The Invalid Table Length error.
It signals that the NAMEd table's length VALUE is less than SMALLEST, or
greater than LARGEST."))

(defun load-tfm-font (stream lf
		      &key (file (when (typep stream 'file-stream)
				   (pathname stream)))
			   (name (when file
				   (pathname-name file)))
		      &aux (font (make-font name :file file)))
  "Parse TFM STREAM of declared length LF into a new font, and return it.
FILE defaults to the STREAM's associated file if any, and NAME defaults to
the FILE's base name, if any."

  ;; 1. Read the rest of the preamble and perform some sanity checks.
  (let ((lh (read-u16 stream t))
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
    (let ((actual-size (file-length stream))
	  (declared-size (* 4 lf)))
      (cond ((< actual-size declared-size)
	     (error 'file-underflow
		    :actual-size actual-size
		    :declared-size declared-size
		    :stream stream))
	    ((> actual-size declared-size)
	     (warn 'file-overflow
		   :actual-size actual-size
		   :declared-size declared-size
		   :stream stream))))
    (unless (>= lh 2) (error 'invalid-header-length :value lh :stream stream))
    (unless (and (<= (1- bc) ec) (<= ec 255))
      (error 'invalid-character-range :bc bc :ec ec :stream stream))
    (when (> bc 255) (setq bc 1 ec 0))
    (setq nc (+ ec (- bc) 1))
    (setf (min-code font) bc (max-code font) ec)
    (unless (= lf (+ 6 lh nc nw nh nd ni nl nk ne np))
      (error 'invalid-section-lengths
	     :lf lf :lh lh :nc nc :nw nw :nh nh :nd nd :ni ni :nl nl :nk nk
	     :ne ne :np np
	     :stream stream))
    (loop :for length :in (list nw nh nd ni ne)
	  :for min :in '(1 1 1 1 0)
	  :for max :in '(256 16 16 64 256)
	  :for name :in '("width" "height" "depth" "italic correction"
			  "exten")
	  :unless (<= min length max)
	    :do (error 'invalid-table-length
		       :smallest min :largest max :value length :name name
		       :stream stream))

    ;; 2. Parse the header section.
    (parse-header stream lh font)

    ;; 3. Parse the 8 character-related sections.
    (parse-character-information stream nc nw nh nd ni nl nk ne font)

    ;; 4. Parse the parameters section.
    (parse-parameters stream np font))
  font)



;; ==========================================================================
;; Entry Point
;; ==========================================================================

(define-condition extended-tfm (tfm-warning)
  ((file :initarg :file :accessor file)
   (extension :initarg :extension :accessor extension))
  (:report (lambda (extended-tfm stream)
	     (format stream "File ~A contains ~A data (not supported yet)."
	       (file extended-tfm)
	       (extension extended-tfm))))
  (:documentation "The Extended TFM warning.
It signals that FILE contains EXTENSION data (OFM or JFM) rather than plain
TFM data."))

(defun load-font (file)
  "Load FILE into a new font, and return it.
Only actual TFM data is currently supported. This function warns and returns
NIL if FILE contains OFM or JFM data."
  (with-open-file
      (stream file :direction :input :element-type '(unsigned-byte 8))
    (let ((lf (read-u16 stream t)))
      (cond ((zerop lf)
	     (warn 'extended-tfm :file file :extension "OFM"))
	    ((or (= lf 9) (= lf 11))
	     (warn 'extended-tfm :file file :extension "JFM"))
	    (t
	     (load-tfm-font stream lf))))))

;;; file.lisp ends here
