;;; character-metrics.lisp --- Character Metrics Information

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
;; Class
;; ==========================================================================

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
    :documentation "The character's italic correction, in design size units.
TeX uses this value for regular characters followed by the command \/, and
also in math mode for superscript placement."
    :initarg :italic-correction
    :reader italic-correction)
   (next-character
    :documentation "The next character in a character list.
This slot is non-null only if the character is part of a chain of characters
of ascending size, and not the last one (see TeX: the Program [544]). It is
mutually exclusive with the EXTENSION-RECIPE slot, and also with the existence
of a ligature or kerning program for this character."
    :initform nil
    :accessor next-character)
   (extension-recipe
    :documentation "The character's extension recipe.
This is an array of top, middle, bottom, and repeated characters. Only the
first 3 may be NIL, meaning that the final character is constructed without
them. This slot is non-null only if this character is extensible (see TeX: the
Program [544]). It is mutually exclusive with the NEXT-CHARACTER slot,
and also with the existence of a ligature or kerning program for this
character."
    :initform nil
    :accessor extension-recipe))
  (:documentation "The Character Metrics class.
This class represents decoded character information. Within the context of
this library, the term \"character\" denotes an instance of this class."))

(defmethod print-object ((character character-metrics) stream)
  "Print CHARACTER unreadably with its code to STREAM."
  (print-unreadable-object (character stream :type t)
    (princ (code character) stream)))

(defun make-character-metrics (code width height depth italic-correction)
  "Make a new CHARACTER-METRICS instance, and return it.
Initialize the character's CODE, WIDTH, HEIGHT, DEPTH, and ITALIC-CORRECTION
appropriately. The two remaining slots (NEXT-CHARACTER and EXTENSION-RECIPE)
will be initialized later if needed, when all character metrics instances are
created."
  (make-instance 'character-metrics
    :code code
    :width width
    :height height
    :depth depth
    :italic-correction italic-correction))



;; ==========================================================================
;; Extension Recipe Pseudo-Accessors
;; ==========================================================================

(defun extensiblep (character)
  "Return T if CHARACTER has an extension recipe."
  ;; We don't want to expose the array itself.
  (when (extension-recipe character) t))

(define-condition not-extensible (tfm-usage-error)
  ((value :initarg :value :accessor value))
  (:report (lambda (not-extensible stream)
	     (format stream "Character ~A is not extensible."
	       (value not-extensible))))
  (:documentation "The Not Extensible error.
It signals an attempt to access a non-extensible character's extension
recipe."))

(defun safe-extension-recipe-access (character index)
  "Return CHARACTER's INDEXth extension recipe value.
If CHARACTER is not extensible, signal a NOT-EXTENSIBLE error."
  (if (extensiblep character)
    (aref (extension-recipe character) index)
    (error 'not-extensible :value character)))

(defun top-character (character)
  "Return the top character in CHARACTER's extension recipe, or NIL."
  (safe-extension-recipe-access character 0))

(defun middle-character (character)
  "Return the middle character in CHARACTER's extension recipe, or NIL."
  (safe-extension-recipe-access character 1))

(defun bottom-character (character)
  "Return the bottom character in CHARACTER's extension recipe, or NIL."
  (safe-extension-recipe-access character 2))

(defun repeated-character (character)
  "Return the repeated character in CHARACTER's extension recipe."
  (safe-extension-recipe-access character 3))

;;; character-metrics.lisp ends here
