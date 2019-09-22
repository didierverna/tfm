;;; font.lisp --- Font Information

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
;; Ligatures
;; ==========================================================================

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
This class represents a decoded ligature program. Within the context of this
library, the term \"ligature\" denotes an instance of this class."))

(defun make-ligature (composite delete-before delete-after pass-over)
  "Make a new LIGATURE instance, and return it."
  (make-instance 'ligature
    :composite composite
    :delete-before delete-before
    :delete-after delete-after
    :pass-over pass-over))



;; ==========================================================================
;; Base Font
;; ==========================================================================

;; -----
;; Class
;; -----

(defclass font ()
  ((name
    :documentation "The font's name."
    :initarg :name
    :reader name)
   (file
    :documentation "The file from which the font was loaded, or NIL."
    :initform nil
    :initarg :file
    :accessor file)
   (checksum :documentation "The font's checksum." :accessor checksum)
   (encoding
    :documentation "The font's character coding scheme, if available."
    :initform nil
    :accessor encoding)
   (family
    :documentation "The font's family, if available."
    :initform nil
    :accessor family)
   (7bits-safe
    :documentation "Whether the font is 7 bits safe, if available.
Note that meaningful values are 0 or 1. A value of NIL means that the
information in unavailable."
    :initform nil
    :accessor 7bits-safe)
   (face-number
    :documentation "The font's face number.
Note that meaningful values are numbers. A value of NIL means that the
information in unavailable. For values less than 18, the slots FACE-CODE,
WEIGHT, SLOPE, and EXPANSION are also filled in."
    :initform nil
    :accessor face-number)
   (face-code
    :documentation "The font's 3-letters face code, or NIL.
A value of NIL means that the information in unavailable (FACE number is NIL
or greater than 17). This code is constructed from the first letters of the
WEIGHT, SLOPE, and EPXANSION slots. That is: [MBL][RI][RCE]."
    :initform nil
    :accessor face-code)
   (weight
    :documentation "The font's weight (:medium, :bold, :light, or NIL).
A value of NIL means that the information in unavailable (FACE number is NIL
or greater than 17)."
    :initform nil
    :accessor weight)
   (slope
    :documentation "The font's slope (:roman, :italic, or NIL).
A value of NIL means that the information in unavailable (FACE number is NIL
or greater than 17)."
    :initform nil
    :accessor slope)
   (expansion
    :documentation
    "The font's expansion (:regular, :condensed, :extended, or NIL).
A value of NIL means that the information in unavailable (FACE number is NIL
or greater than 17)."
    :initform nil
    :accessor expansion)
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
   (boundary-character
    :documentation "The font's boundary character, if any.
This character is also accessible by code, like normal ones. However, it is
the only character the code of which may be outside [MIN-CODE,MAX-CODE] (see
TeX: the Program [545]). Finally, this character is not included in the
character count, unless it exists for real in the font."
    :initform nil
    :accessor boundary-character)
   (parameters
    :documentation "The font's additional parameters array, if any."
    :initform nil
    :accessor parameters))
  (:documentation "The TeX Font Metrics class.
This class represents decoded font information. Within the context of this
library, the term \"font\" denotes an instance of this class, or of one of its
subclasses."))

(defmethod print-object ((font font) stream)
  "Print FONT unreadably with its name to STREAM."
  (print-unreadable-object (font stream :type t)
    (princ (name font) stream)))

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
  ((value :initarg :value :accessor value))
  (:report (lambda (invalid-character-code stream)
	     (report stream "character code ~A is invalid."
		     (value invalid-character-code))))
  (:documentation "The Invalid Character Code error.
It signals an invalid code VALUE for this font."))

;; #### NOTE: this is the internal API, used while loading TFM data.
(defun code-character (code font &optional (errorp t))
  "Return FONT's CODE character.
If ERRORP (the default), signal an error if CODE is invalid. Note that even a
fake boundary character may be retrieved by this function"
  (or (gethash code (characters font))
      (when errorp (error 'invalid-character-code :value code))))

(defun (setf code-character) (character font)
  "Make FONT's CHARACTER accessible by its code."
  (setf (gethash (code character) (characters font)) character))

;; #### NOTE: this is the public API.
(defun get-character (code font &optional (errorp t))
  "Return FONT's CODE character, or NIL if there is not such character."
  (gethash code (characters font)))

;; #### NOTE: we don't bother to make a distinction between internal and
;; external API here, because these functions use characters, not character
;; codes (hence, an error would be signalled when characters are retrieved by
;; the internal API).
(defun ligature (character1 character2 font)
  "Return FONT's ligature for CHARACTER1 and CHARACTER2 if any."
  (gethash (cons character1 character2) (ligatures font)))

(defun (setf ligature) (ligature character1 character2 font)
  "Set FONT's LIGATURE for CHARACTER1 and CHARACTER2."
  (setf (gethash (cons character1 character2) (ligatures font)) ligature))

(defun kerning (character1 character2 font)
  "Return FONT's kerning for CHARACTER1 and CHARACTER2 if any."
  (gethash (cons character1 character2) (kernings font)))

(defun (setf kerning) (kerning character1 character2 font)
  "Set FONT's KERNING for CHARACTER1 and CHARACTER2."
  (setf (gethash (cons character1 character2) (kernings font)) kerning))



;; ==========================================================================
;; Math Symbols Font
;; ==========================================================================

(defclass math-symbols-font (font)
  ((num1
    :documentation "The font's NUM1 parameter."
    :initform 0
    :accessor num1)
   (num2
    :documentation "The font's NUM2 parameter."
    :initform 0
    :accessor num2)
   (num3
    :documentation "The font's NUM2 parameter."
    :initform 0
    :accessor num3)
   (denom1
    :documentation "The font's DENOM1 parameter."
    :initform 0
    :accessor denom1)
   (denom2
    :documentation "The font's DENOM2 parameter."
    :initform 0
    :accessor denom2)
   (sup1
    :documentation "The font's SUP1 parameter."
    :initform 0
    :accessor sup1)
   (sup2
    :documentation "The font's SUP2 parameter."
    :initform 0
    :accessor sup2)
   (sup3
    :documentation "The font's SUP2 parameter."
    :initform 0
    :accessor sup3)
   (sub1
    :documentation "The font's SUB1 parameter."
    :initform 0
    :accessor sub1)
   (sub2
    :documentation "The font's SUB2 parameter."
    :initform 0
    :accessor sub2)
   (supdrop
    :documentation "The font's SUPDROP parameter."
    :initform 0
    :accessor supdrop)
   (subdrop
    :documentation "The font's SUBDROP parameter."
    :initform 0
    :accessor subdrop)
   (delim1
    :documentation "The font's DELIM1 parameter."
    :initform 0
    :accessor delim1)
   (delim2
    :documentation "The font's DELIM2 parameter."
    :initform 0
    :accessor delim2)
   (axis-height
    :documentation "The font's AXIS-HEIGHT parameter."
    :initform 0
    :accessor axis-height))
  (:documentation "The Math Symbols Font class.
This class represents fonts with the \"TeX math symbols\" character coding
scheme."))



;; ==========================================================================
;; Math Symbols Font
;; ==========================================================================

(defclass math-extension-font (font)
  ((default-rule-thickness
    :documentation "The font's default rule thickness."
    :initform 0
    :accessor default-rule-thickness)
   (big-op-spacing1
    :documentation "The font's BIG-OP-SPACING1 parameter."
    :initform 0
    :accessor big-op-spacing1)
   (big-op-spacing2
    :documentation "The font's BIG-OP-SPACING2 parameter."
    :initform 0
    :accessor big-op-spacing2)
   (big-op-spacing3
    :documentation "The font's BIG-OP-SPACING3 parameter."
    :initform 0
    :accessor big-op-spacing3)
   (big-op-spacing4
    :documentation "The font's BIG-OP-SPACING4 parameter."
    :initform 0
    :accessor big-op-spacing4)
   (big-op-spacing5
    :documentation "The font's BIG-OP-SPACING5 parameter."
    :initform 0
    :accessor big-op-spacing5))
  (:documentation "The Math Extension Font class.
This class represents fonts with the \"TeX math extension\" character coding
scheme."))

;;; font.lisp ends here
