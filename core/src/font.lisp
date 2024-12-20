;;; font.lisp --- Font Information

;; Copyright (C) 2018, 2019, 2024 Didier Verna

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
;; Base Font
;; ==========================================================================

;; -----
;; Class
;; -----

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +font-dimension-accessors+
      '(interword-space interword-stretch interword-shrink ex em extra-space)
    "The list of dimension accessor names in the FONT class."))

(defmacro map-font-dimension-accessors (var font &body body)
  "Map BODY on FONT dimension accessors available as VAR."
  `(map-accessors ,var ,font ,+font-dimension-accessors+
     ,@body))

(defclass font ()
  ((name
    :documentation "The font's name.
When the font is loaded from a file, it defaults to the file's base name,
along with potential scaling information."
    :initarg :name
    :accessor name)
   (file
    :documentation "The file from which the font was loaded, or NIL."
    :initform nil
    :initarg :file
    :accessor file)
   (checksum
    :documentation "The font's checksum, as provided by Metafont."
    :accessor checksum)
   (frozen
    :documentation "Whether the font is frozen."
    :initform nil
    :accessor frozen)
   (design-size
    :documentation "The font's design size, in units of TeX points."
    :initform nil
    :accessor design-size)
   (original-design-size
    :documentation "The font's original design size, in units of TeX points."
    :accessor original-design-size)
   (encoding
    :documentation "The font's character coding scheme (a BCPL string), or NIL."
    :initform nil
    :accessor encoding)
   (family
    :documentation "The font's family (a BCPL string), or NIL."
    :initform nil
    :accessor family)
   (7bits-safe
    :documentation "Whether the font is 7 bits safe (0 or 1), or NIL.
When 1, it means that no character of code lesser than 128 can lead to a
character of code greater than 128 by ways of ligatures or extensible
recipes."
    :initform nil
    :accessor 7bits-safe)
   (face-number
    :documentation "The font's face number, or NIL."
    :initform nil
    :accessor face-number)
   (weight
    :documentation "The font's weight (:medium, :bold, :light), or NIL.
When available, it is decoded from the font face number."
    :initform nil
    :accessor weight)
   (slope
    :documentation "The font's slope (:roman, :italic), or NIL.
When available, it is decoded from the font face number."
    :initform nil
    :accessor slope)
   (expansion
    :documentation
    "The font's expansion (:regular, :condensed, :extended), or NIL.
When available, it is decoded from the font face number."
    :initform nil
    :accessor expansion)
   (face-code
    :documentation "The font's 3-letters face code, or NIL.
When available, it is the concatenation of the upcased first letters of the
font's weight, slope, and expansion."
    :initform nil
    :accessor face-code)
   (slant
    :documentation "The font's slant (a scalar ratio)."
    :initform 0
    :accessor slant)
   (interword-space
    :documentation "The font's normal interword space.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor interword-space)
   (interword-stretch
    :documentation "The font's interword stretchability.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor interword-stretch)
   (interword-shrink
    :documentation "The font's interword shrinkability.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor interword-shrink)
   (ex
    :documentation "The font's ex size.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor ex)
   (em
    :documentation "The font's em size.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor em)
   (extra-space
    :documentation "The font's extra space.
It is expressed in design size units, or in TeX point units if the font is
frozen.

This is the additional space to put at the end of sentences."
    :initform 0 :accessor extra-space)
   (parameters
    :documentation "The font's additional parameters array, or NIL.
Parameters are expressed in design size units, or in TeX point units if the
font is frozen."
    :initform nil
    :accessor parameters)
   (min-code
    :documentation
    "The font's smallest character code, or NIL if the font is empty."
    :initform nil
    :accessor min-code)
   (max-code
    :documentation
    "The font's largest character code, or NIL if the font is empty."
    :initform nil
    :accessor max-code)
   (characters
    :documentation "The font's characters.
This is a hash table associating character codes with characters."
    :initform (make-hash-table :test #'eq)
    :accessor characters)
   (character-count
    :documentation "The font's number of characters.
The character count does not include the boundary character, unless that
character really exists in the font (has non-zerop metrics)."
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
kerning. They are expressed in design size units, or in TeX point units if
the font is frozen."
    :initform (make-hash-table :test #'equal)
    :accessor kernings)
   (boundary-character
    :documentation "The font's boundary character, or NIL.
This character is also accessible by code, like normal ones. However, it is
the only character the code of which may be outside [MIN-CODE,MAX-CODE] (see
TeX: the Program [545]). Finally, this character is not included in the
character count, unless it exists for real in the font (has non-zero
metrics)."
    :initform nil
    :accessor boundary-character))
  (:documentation "The TeX Font Metrics class.
This class represents decoded font information. Within the context of this
library, the term \"font\" denotes an instance of this class, or of one of its
subclasses."))

(defmethod (setf design-size) :before (design-size font)
  "Unscale FONT if frozen."
  (when (frozen font) (scale font (/ 1 (design-size font)))))

(defmethod (setf design-size) :after (design-size font)
  "Rescale FONT if frozen."
  (when (frozen font) (scale font (design-size font))))

(defmethod (setf design-size) :around (design-size font)
  "Check that DESIGN-SIZE is a real greater or equal to 1."
  (check-type design-size (real 1))
  (call-next-method design-size font))

(defmethod print-object ((font font) stream)
  "Print FONT unreadably with its name to STREAM."
  (print-unreadable-object (font stream :type t)
    (princ (name font) stream)
    (unless (= (design-size font) (original-design-size font))
      (format stream " at ~Apt" (design-size font)))
    (when (frozen font) (princ " (frozen)" stream))))

;; #### NOTE: this error is not currently exported, because it cannot in fact
;; be triggered yet (by the public API).
(define-condition anonymous-font (tfm-usage-error)
  ()
  (:documentation "The Anonymous Font usage error.
It signals an attempt at creating a font with no name."))

(define-condition-report (condition anonymous-font)
  "all fonts must be named.")


(defmethod initialize-instance :before ((font font) &key name)
  "Check that FONT has a name, or signal an ANONYMOUS-FONT error."
  (unless name (error 'anonymous-font)))

(defun make-font (name &rest initargs)
  "Make a new NAMEd FONT instance, and return it.
If INITARGS are provided, pass them as-is to MAKE-INSTANCE."
  (apply #'make-instance 'font :name name initargs))


;; ----------------
;; Pseudo-accessors
;; ----------------

;; #### NOTE: this is a compliance error. It may only be signalled by the
;; internal API, meaning that the TFM data contains invalid references to
;; characters that don't exist in the font (remember that we do add a fake
;; boundary character if needed, so even this one can be retrieved).
(define-condition invalid-character-code (tfm-compliance-error)
  ((value
    :documentation "The invalid character code."
    :initarg :value
    :accessor value))
  (:documentation "The Invalid Character Code compliance error.
It signals a reference to a character code which does not exist in the font
being loaded."))

(define-condition-report (condition invalid-character-code)
  "character code ~A is invalid."
  (value condition))


;; #### NOTE: this is the internal API, used while loading TFM data.
(defun code-character (code font &optional (errorp t))
  "Return FONT's CODE character.
If ERRORP (the default), check that the character exists, or signal an
INVALID-CHARACTER-CODE error. Note that a fake boundary character may be
retrieved by this function."
  (or (gethash code (characters font))
      ;; #### NOTE: recovering from here directly would make little sense, so
      ;; it's rather the job of the callers to provide sensible restarts.
      (when errorp (error 'invalid-character-code :value code))))

(defun (setf code-character) (character font)
  "Make FONT's CHARACTER accessible by its code."
  (setf (gethash (code character) (characters font)) character))

;; #### NOTE: this is the public API.
(defun get-character (code font)
  "Return FONT's CODE character, or NIL."
  (gethash code (characters font)))


(define-condition different-fonts (tfm-usage-error)
  ((character1
    :documentation "The first character."
    :initarg :character1
    :accessor character1)
   (character2
    :documentation "The second character."
    :initarg :character2
    :accessor character2))
  (:documentation "The Different Fonts usage error.
It signals an attempt at retrieving a ligature or kerning for two characters
from different fonts."))

(define-condition-report (condition different-fonts)
  "characters ~A and ~A don't belong to the same font."
  (character1 condition)
  (character2 condition))


(defun ligature (character1 character2)
  "Return ligature for CHARACTER1 and CHARACTER2, or NIL.
If CHARACTER1 and CHARACTER2 don't belong to the same font, signal a
DIFFERENT-FONTS error."
  (unless (eq (font character1) (font character2))
    (error 'different-fonts :character1 character1 :character2 character2))
  (gethash (cons character1 character2) (ligatures (font character1))))

(defun kerning (character1 character2)
  "Return kerning for CHARACTER1 and CHARACTER2, or NIL.
If CHARACTER1 and CHARACTER2 don't belong to the same font, signal a
DIFFERENT-FONTS error."
  (unless (eq (font character1) (font character2))
    (error 'different-fonts :character1 character1 :character2 character2))
  (gethash (cons character1 character2) (kernings (font character1))))


;; #### NOTE: we don't currently bother to check that the two characters
;; belong to the same font. These functions are internal only (although the
;; symbols are exported, damn you CL), so let's just say I trust my own code
;; for now.
(defun (setf ligature) (ligature character1 character2)
  "Set LIGATURE for CHARACTER1 and CHARACTER2."
  (setf (gethash (cons character1 character2) (ligatures (font character1)))
	ligature))

(defun (setf kerning) (kerning character1 character2)
  "Set KERNING for CHARACTER1 and CHARACTER2."
  (setf (gethash (cons character1 character2) (kernings (font character1)))
	kerning))


;; --------
;; Freezing
;; --------

(defgeneric scale (font factor)
  (:documentation "Scale all FONT dimensions by FACTOR.")
  (:method (font factor)
    "Scaling method for regular FONTs."
    (map-font-dimension-accessors slot font
      (setf slot (* slot factor)))
    (when (parameters font)
      (loop :for i :from 0 :upto (1- (length (parameters font)))
	    :do (setf (aref (parameters font) i)
		      (* (aref (parameters font) i) factor))))
    (maphash (lambda (code character)
	       (declare (ignore code))
	       (map-character-metrics-dimension-accessors slot character
		 (setf slot (* slot factor))))
	     (characters font))
    (maphash (lambda (pair kern)
	       (setf (kerning (car pair) (cdr pair)) (* kern factor)))
	     (kernings font))))

(defun freeze (font)
  "Freeze FONT.
Freezing a font means that all dimensions normally expressed in design size
units are multiplied by it, so as to lead values in TeX point units.
If FONT is already frozen, this function does nothing and returns NIL.
Otherwise, it returns T."
  (unless (frozen font)
    (scale font (design-size font))
    (setf (frozen font) t)))

(defun unfreeze (font)
  "Unfreeze FONT.
Unfreezing means performing the inverse of what FREEZE does.
If FONT is not frozen, this function does nothing and returns NIL. Otherwise,
it returns T."
  (when (frozen font)
    (scale font (/ 1 (design-size font)))
    (setf (frozen font) nil)
    t))



;; ==========================================================================
;; Math Symbols Font
;; ==========================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +math-symbols-font-dimension-accessors+
      '(num1 num2 num3
	denom1 denom2
	sup1 sup2 sup3
	sub1 sub2
	supdrop subdrop
	delim1 delim2
	axis-height)
    "The list of dimension accessor names in the MATH-SYMBOLS-FONT class."))

(defmacro map-math-symbols-font-dimension-accessors (var font &body body)
  "Map BODY on FONT dimension accessors available as VAR."
  `(map-accessors ,var ,font ,+math-symbols-font-dimension-accessors+
     ,@body))

(defclass math-symbols-font (font)
  ((num1
    :documentation "The font's NUM1 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor num1)
   (num2
    :documentation "The font's NUM2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor num2)
   (num3
    :documentation "The font's NUM2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor num3)
   (denom1
    :documentation "The font's DENOM1 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor denom1)
   (denom2
    :documentation "The font's DENOM2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor denom2)
   (sup1
    :documentation "The font's SUP1 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor sup1)
   (sup2
    :documentation "The font's SUP2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor sup2)
   (sup3
    :documentation "The font's SUP2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor sup3)
   (sub1
    :documentation "The font's SUB1 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor sub1)
   (sub2
    :documentation "The font's SUB2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor sub2)
   (supdrop
    :documentation "The font's SUPDROP parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor supdrop)
   (subdrop
    :documentation "The font's SUBDROP parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor subdrop)
   (delim1
    :documentation "The font's DELIM1 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor delim1)
   (delim2
    :documentation "The font's DELIM2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor delim2)
   (axis-height
    :documentation "The font's AXIS-HEIGHT parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor axis-height))
  (:documentation "The Math Symbols Font class.
This class represents fonts with the \"TeX math symbols\" character coding
scheme."))

(defmethod scale :around ((font math-symbols-font) factor)
  "Scaling method for MATH-SYMBOL-FONTs."
  (map-math-symbols-font-dimension-accessors slot font
    (setf slot (* slot factor)))
  (call-next-method))



;; ==========================================================================
;; Math Symbols Font
;; ==========================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +math-extension-font-dimension-accessors+
      '(default-rule-thickness
	big-op-spacing1 big-op-spacing2 big-op-spacing3 big-op-spacing4
	big-op-spacing5)
    "The list of dimension accessor names in the MATH-EXTENSION-FONT class."))

(defmacro map-math-extension-font-dimension-accessors (var font &body body)
  "Map BODY on math extension FONT dimension accessors available as VAR."
  `(map-accessors ,var ,font ,+math-extension-font-dimension-accessors+
     ,@body))

(defclass math-extension-font (font)
  ((default-rule-thickness
    :documentation "The font's default rule thickness.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor default-rule-thickness)
   (big-op-spacing1
    :documentation "The font's BIG-OP-SPACING1 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor big-op-spacing1)
   (big-op-spacing2
    :documentation "The font's BIG-OP-SPACING2 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor big-op-spacing2)
   (big-op-spacing3
    :documentation "The font's BIG-OP-SPACING3 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor big-op-spacing3)
   (big-op-spacing4
    :documentation "The font's BIG-OP-SPACING4 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor big-op-spacing4)
   (big-op-spacing5
    :documentation "The font's BIG-OP-SPACING5 parameter.
It is expressed in design size units, or in TeX point units if the font is
frozen."
    :initform 0
    :accessor big-op-spacing5))
  (:documentation "The Math Extension Font class.
This class represents fonts with the \"TeX math extension\" character coding
scheme."))

(defmethod scale :around ((font math-extension-font) factor)
  "Scaling method for MATH-EXTENSION-FONTs."
  (map-math-extension-font-dimension-accessors slot font
    (setf slot (* slot factor)))
  (call-next-method))

;;; font.lisp ends here
