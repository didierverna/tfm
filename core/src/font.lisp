;;; font.lisp --- Font Information

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



;;; Code:

(in-package :net.didierverna.tfm)
(in-readtable :net.didierverna.tfm)


;; ==========================================================================
;; Ligatures
;; ==========================================================================

(defclass ligature ()
  ((composite
    :documentation "The character to insert between the two original ones."
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
This class represents a decoded ligature program. Within the context of this
library, the term \"ligature\" denotes an instance of this class."))

;; #### NOTE: the pass-over must not exceed the number of original characters
;; retained, but this has been checked already by the (unique) caller when
;; decoding the ligature op-code.
(defun make-ligature (composite delete-before delete-after pass-over)
  "Make a new LIGATURE instance, and return it."
  (make-instance 'ligature
    :composite composite
    :delete-before delete-before
    :delete-after delete-after
    :pass-over pass-over))

(defun apply-ligature (ligature state &aux (new-state (cddr state)))
  "Apply LIGATURE to STATE and return the new state.
STATE is a list of characters, the first two being subject to LIGATURE."
  (unless (delete-after ligature) (push (cadr state) new-state))
  (push (composite ligature) new-state)
  (unless (delete-before ligature) (push (car state) new-state))
  (cond ((= (pass-over ligature) 2) (cddr new-state))
	((= (pass-over ligature) 1) (cdr new-state))
	((= (pass-over ligature) 0) new-state)))




;; ==========================================================================
;; Base (TFM) Font
;; ==========================================================================

;; -----
;; Class
;; -----

(defclass font ()
  ((file
    :documentation "The font's file."
    :initarg :file
    :reader file)
   (name
    :documentation "The font's name."
    :initform nil
    :initarg :name
    :reader name)
   (checksum
    :documentation "The font's checksum."
    :reader checksum)
   (frozen
    :documentation "Whether the font is frozen."
    :initform nil
    :reader frozen)
   (design-size
    :documentation "The font's design size, in units of TeX points."
    :initform nil
    :initarg :design-size
    :accessor design-size)
   (original-design-size
    :documentation "The font's original design size, in units of TeX points."
    :reader original-design-size)
   (encoding
    :documentation "The font's character coding scheme (a BCPL string), or NIL."
    :initform nil
    :reader encoding)
   (family
    :documentation "The font's family (a BCPL string), or NIL."
    :initform nil
    :reader family)
   (7bits-safe
    :documentation "Whether the font is 7 bits safe (0 or 1), or NIL.
When 1, it means that no character of code lesser than 128 can lead to a
character of code greater than 128 by ways of ligatures or extensible
recipes."
    :initform nil
    :reader 7bits-safe)
   (face-number
    :documentation "The font's face number, or NIL."
    :initform nil
    :reader face-number)
   (weight
    :documentation "The font's weight (:medium, :bold, :light), or NIL.
When available, it is decoded from the font face number."
    :initform nil
    :reader weight)
   (slope
    :documentation "The font's slope (:roman, :italic), or NIL.
When available, it is decoded from the font face number."
    :initform nil
    :reader slope)
   (expansion
    :documentation
    "The font's expansion (:regular, :condensed, :extended), or NIL.
When available, it is decoded from the font face number."
    :initform nil
    :reader expansion)
   (face-code
    :documentation "The font's 3-letters face code, or NIL.
When available, it is the concatenation of the upcased first letters of the
font's weight, slope, and expansion."
    :initform nil
    :reader face-code)
   (slant
    :documentation "The font's slant (a scalar ratio)."
    :initform 0
    :reader slant)
   (interword-space
    :documentation "The font's normal interword space.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader interword-space)
   (interword-stretch
    :documentation "The font's interword stretchability.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader interword-stretch)
   (interword-shrink
    :documentation "The font's interword shrinkability.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader interword-shrink)
   (ex
    :documentation "The font's ex size.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader ex)
   (em
    :documentation "The font's em size.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader em)
   (extra-space
    :documentation "The font's extra space.
It is expressed in design size units, or in TeX point units if the font is
frozen.

This is the additional space to put at the end of sentences."
    :initform 0
    :reader extra-space)
   (parameters
    :documentation "The font's additional parameters array, or NIL.
Parameters are expressed in design size units, or in TeX point units if the
font is frozen."
    :initform nil
    :reader parameters)
   (min-code
    :documentation
    "The font's smallest character code, or NIL if the font is empty."
    :initform nil
    :reader min-code)
   (max-code
    :documentation
    "The font's largest character code, or NIL if the font is empty."
    :initform nil
    :reader max-code)
   (characters
    :documentation "The font's characters.
This is a hash table associating character codes with characters."
    :initform (make-hash-table :test #'eq)
    :reader characters)
   (character-count
    :documentation "The font's number of characters.
The character count does not include the boundary character, unless that
character really exists in the font (has non-zero metrics)."
    :reader character-count)
   (ligatures
    :documentation "The font's ligatures.
This is a hash table associating conses of characters with the corresponding
ligature."
    :initform (make-hash-table :test #'equal)
    :reader ligatures)
   (kerns
    :documentation "The font's kerns.
This is a hash table associating conses of characters with the corresponding
kern. They are expressed in design size units, or in TeX point units if
the font is frozen."
    :initform (make-hash-table :test #'equal)
    :reader kerns)
   (boundary-character
    :documentation "The font's boundary character, or NIL.
This character is also accessible by code, like normal ones. However, it is
the only character the code of which may be outside [MIN-CODE,MAX-CODE] (see
TeX: the Program [545]). Finally, this character is not included in the
character count, unless it exists for real in the font (has non-zero
metrics)."
    :initform nil
    :reader boundary-character))
  (:documentation "The TeX Font Metric class.
This class represents decoded font information. Within the context of this
library, the term \"font\" denotes an instance of this class, or of one of its
subclasses."))


(defmethod print-object ((font font) stream)
  "Print FONT unreadably with its name to STREAM."
  (print-unreadable-object (font stream :type t)
    (princ (name font) stream)
    (unless (= (design-size font) (original-design-size font))
      (format stream " at ~Apt" (design-size font)))
    (when (frozen font) (princ " (frozen)" stream))))


;; #### NOTE: we're not currently so pedantic as to check that the font's file
;; has a non-empty base name.
(defmethod initialize-instance :after ((font font) &key)
  "Handle FONT's name initialization.
Unless a custom name has been provided already, initialize FONT's name to the
font file's base name."
  (with-slots (file name) font
    ;; #### NOTE: the validity of a custom name has already been checked by
    ;; LOAD-FONT.
    (unless name (setq name (pathname-name file)))))


;; ---------------------------------------------
;; Internal character, ligature, and kern access
;; ---------------------------------------------

;; #### NOTE: this is a compliance error. It may only be signalled by the
;; internal API, meaning that the TFM data contains invalid references to
;; characters that don't exist in the font (remember that we do add a fake
;; boundary character if needed, so even this one can be retrieved).
(define-condition invalid-character-code (tfm-compliance-error)
  ((code
    :documentation "The invalid character code."
    :initarg :code
    :reader code))
  (:documentation "The Invalid Character Code compliance error.
It signals a reference to a character code which does not exist in the font
being loaded."))

(define-condition-report (condition invalid-character-code)
    "character code ~A is invalid."
  (code condition))

(defun code-character (code font &optional (errorp t))
  "Return FONT's CODE character.
If ERRORP (the default), check that the character exists, or signal an
INVALID-CHARACTER-CODE error. Note that a fake boundary character may be
retrieved by this function."
  (or (gethash code (characters font))
      ;; #### NOTE: recovering from here directly would make little sense, so
      ;; it's rather the job of the callers to provide sensible restarts.
      (when errorp (error 'invalid-character-code :code code))))


(defun (setf code-character) (character font)
  "Make FONT's CHARACTER accessible by its code."
  (setf (gethash (code character) (characters font)) character))

;; #### NOTE: we don't currently bother to check that the two characters
;; belong to the same font. These functions are internal only, so let's just
;; say I trust my own code for now.
(defun set-ligature (character1 character2 ligature)
  "Set LIGATURE for CHARACTER1 and CHARACTER2."
  (setf (gethash (cons character1 character2) (ligatures (font character1)))
	ligature))

(defun set-kern (character1 character2 kern)
  "Set KERN for CHARACTER1 and CHARACTER2."
  (setf (gethash (cons character1 character2) (kerns (font character1)))
	kern))


;; -------
;; Scaling
;; -------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +font-dimension-slots+
      '(interword-space interword-stretch interword-shrink ex em extra-space)
    "The list of dimension slot names in the FONT class."))

(defmacro map-font-dimension-slots (var font &body body)
  "Map BODY on FONT dimension slots available as VAR."
  `(map-slots ,var ,font ,+font-dimension-slots+ ,@body))

(defgeneric scale (font factor)
  (:documentation "Scale all FONT dimensions by FACTOR.")
  (:method (font factor)
    "Scaling method for regular FONTs."
    (map-font-dimension-slots slot font
      (setf slot (* slot factor)))
    (when (parameters font)
      (loop :for i :from 0 :upto (1- (length (parameters font)))
	    :do (setf (aref (parameters font) i)
		      (* (aref (parameters font) i) factor))))
    (maphash (lambda (code character)
	       (declare (ignore code))
	       (map-character-metrics-dimension-slots slot character
		 (setf slot (* slot factor))))
	     (characters font))
    (maphash (lambda (pair kern)
	       (set-kern (car pair) (cdr pair) (* kern factor)))
	     (kerns font))))




;; ==========================================================================
;; Math Symbols Font
;; ==========================================================================

(defclass math-symbols-font (font)
  ((num1
    :documentation "The font's NUM1 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader num1)
   (num2
    :documentation "The font's NUM2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader num2)
   (num3
    :documentation "The font's NUM2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader num3)
   (denom1
    :documentation "The font's DENOM1 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader denom1)
   (denom2
    :documentation "The font's DENOM2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader denom2)
   (sup1
    :documentation "The font's SUP1 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader sup1)
   (sup2
    :documentation "The font's SUP2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader sup2)
   (sup3
    :documentation "The font's SUP2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader sup3)
   (sub1
    :documentation "The font's SUB1 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader sub1)
   (sub2
    :documentation "The font's SUB2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader sub2)
   (supdrop
    :documentation "The font's SUPDROP parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader supdrop)
   (subdrop
    :documentation "The font's SUBDROP parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader subdrop)
   (delim1
    :documentation "The font's DELIM1 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader delim1)
   (delim2
    :documentation "The font's DELIM2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader delim2)
   (axis-height
    :documentation "The font's AXIS-HEIGHT parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader axis-height))
  (:documentation "The Math Symbols Font class.
This class represents fonts with the \"TeX math symbols\" character coding
scheme."))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +math-symbols-font-dimension-slots+
      '(num1 num2 num3
	denom1 denom2
	sup1 sup2 sup3
	sub1 sub2
	supdrop subdrop
	delim1 delim2
	axis-height)
    "The list of dimension slot names in the MATH-SYMBOLS-FONT class."))

(defmacro map-math-symbols-font-dimension-slots (var font &body body)
  "Map BODY on FONT dimension slots available as VAR."
  `(map-slots ,var ,font ,+math-symbols-font-dimension-slots+ ,@body))


(defmethod scale :around ((font math-symbols-font) factor)
  "Scaling method for MATH-SYMBOL-FONTs."
  (call-next-method)
  (map-math-symbols-font-dimension-slots slot font
    (setf slot (* slot factor))))




;; ==========================================================================
;; Math Extension Font
;; ==========================================================================

(defclass math-extension-font (font)
  ((default-rule-thickness
    :documentation "The font's default rule thickness.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader default-rule-thickness)
   (big-op-spacing1
    :documentation "The font's BIG-OP-SPACING1 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader big-op-spacing1)
   (big-op-spacing2
    :documentation "The font's BIG-OP-SPACING2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader big-op-spacing2)
   (big-op-spacing3
    :documentation "The font's BIG-OP-SPACING3 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader big-op-spacing3)
   (big-op-spacing4
    :documentation "The font's BIG-OP-SPACING4 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader big-op-spacing4)
   (big-op-spacing5
    :documentation "The font's BIG-OP-SPACING5 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :reader big-op-spacing5))
  (:documentation "The Math Extension Font class.
This class represents fonts with the \"TeX math extension\" character coding
scheme."))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +math-extension-font-dimension-slots+
      '(default-rule-thickness
	big-op-spacing1 big-op-spacing2 big-op-spacing3 big-op-spacing4
	big-op-spacing5)
    "The list of dimension slot names in the MATH-EXTENSION-FONT class."))

(defmacro map-math-extension-font-dimension-slots (var font &body body)
  "Map BODY on math extension FONT dimension slots available as VAR."
  `(map-slots ,var ,font ,+math-extension-font-dimension-slots+ ,@body))


(defmethod scale :around ((font math-extension-font) factor)
  "Scaling method for MATH-EXTENSION-FONTs."
  (call-next-method)
  (map-math-extension-font-dimension-slots slot font
    (setf slot (* slot factor))))




;; ==========================================================================
;; Public API
;; ==========================================================================

;; -------
;; Scaling
;; -------

(defmethod (setf design-size) :before (design-size font)
  "Unscale FONT if frozen."
  (when (frozen font) (scale font (/ 1 (design-size font)))))

(defmethod (setf design-size) :after (design-size font)
  "Rescale FONT if frozen."
  (when (frozen font) (scale font (design-size font))))


(define-condition invalid-custom-design-size (tfm-usage-error)
  ((value
    :documentation "The invalid custom design size."
    :initarg :value
    :reader value))
  (:documentation "The Invalid Custom Design Size usage error.
It signals that a custom design size is not a real greater or equal to 1."))

(define-condition-report (condition invalid-custom-design-size)
    "custom design size ~A is invalid (should be a real greater or equal to 1)"
  (value condition))


(defmethod (setf design-size) :around (design-size font)
  "Check that DESIGN-SIZE is a real greater or equal to 1.
Otherwise, signal and INVALID-CUSTOM-DESIGN-SIZE error. When the font's
original design size is itself valid, this error is immediately restartable
with USE-ORIGINAL-DESIGN-SIZE."
  (unless (typep design-size '(real 1))
    (restart-case (error 'invalid-custom-design-size :value design-size)
      (use-original-design-size ()
	:report "Use the font's original design size."
	:test (lambda (condition)
		(declare (ignore condition))
		(typep (original-design-size font) '(real 1)))
	(setq design-size (original-design-size font)))))
  (call-next-method design-size font))


;; ------------------------------------
;; Character, ligature, and kern access
;; ------------------------------------

(defun get-character (code font)
  "Return FONT's CODE character, or NIL."
  (gethash code (characters font)))


(define-condition different-fonts (tfm-usage-error)
  ((character1
    :documentation "The first character."
    :initarg :character1
    :reader character1)
   (character2
    :documentation "The second character."
    :initarg :character2
    :reader character2))
  (:documentation "The Different Fonts usage error.
It signals an attempt at retrieving a ligature or kern for two characters
from different fonts."))

(define-condition-report (condition different-fonts)
    "characters ~A and ~A don't belong to the same font."
  (character1 condition)
  (character2 condition))


(defun get-ligature (character1 character2)
  "Return ligature for CHARACTER1 and CHARACTER2, or NIL.
If CHARACTER1 and CHARACTER2 don't belong to the same font, signal a
DIFFERENT-FONTS error."
  (unless (eq (font character1) (font character2))
    (error 'different-fonts :character1 character1 :character2 character2))
  (gethash (cons character1 character2) (ligatures (font character1))))


(defun get-kern (character1 character2)
  "Return kern for CHARACTER1 and CHARACTER2, or NIL.
If CHARACTER1 and CHARACTER2 don't belong to the same font, signal a
DIFFERENT-FONTS error."
  (unless (eq (font character1) (font character2))
    (error 'different-fonts :character1 character1 :character2 character2))
  (gethash (cons character1 character2) (kerns (font character1))))


;; --------
;; Freezing
;; --------

(defun freeze (font)
  "Freeze FONT, and return it.
Freezing a font means that all dimensions normally expressed in design size
units are multiplied by it, so as to lead values in TeX point units.
If FONT is already frozen, this function does nothing.
Otherwise, it returns T as a second value."
  (values font
	  (unless (frozen font)
	    (scale font (design-size font))
	    (setf (slot-value font 'frozen) t)
	    t)))

(defun unfreeze (font)
  "Unfreeze FONT and return it.
Unfreezing means performing the inverse of what FREEZE does.
If FONT is not frozen, this function does nothing.
Otherwise, it returns T as a second value."
  (values font
	  (when (frozen font)
	    (scale font (/ 1 (design-size font)))
	    (setf (slot-value font 'frozen) nil)
	    t)))



;; ==========================================================================
;; Level 0 Omega Font
;; ==========================================================================

;; -----
;; Class
;; -----

(defclass ofm0-font (font)
  ((direction :documentation "The font direction." :reader direction))
  (:documentation "The Level 0 Omega Font Metric class."))

;;; font.lisp ends here
