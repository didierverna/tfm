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



;; ==========================================================================
;; Character Information Tables
;; ==========================================================================

;; -------------------------
;; Ligature/Kerning Programs
;; -------------------------

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


;; -----------------
;; Extension Recipes
;; -----------------

(defun make-extension-recipe (exten tfm)
  "Make an extension recipe based on EXTEN and TFM."
  (let ((recipe (make-array 4 :initial-element nil)))
    (loop :for code :in (list (top exten) (mid exten) (bot exten))
	  :for index :from 0
	  :unless (zerop code)
	    :do (setf (aref recipe index) (character-by-code code tfm t)))
    (setf (aref recipe 3) (character-by-code (rep exten) tfm t))
    recipe))


;; ---------------------
;; Character Information
;; ---------------------

(defun parse-character-information (stream nc nw nh nd ni nl nk ne tfm)
  "Parse the 8 character information tables from STREAM into TFM."
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



;; ==========================================================================
;; Parameters Section
;; ==========================================================================

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
  "Parse FILE into a new TFM instance, and return it."
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

;;; file.lisp ends here
