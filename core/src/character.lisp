;;; character.lisp --- Character Information

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
;; Extension Recipes
;; ==========================================================================

(defclass extension-recipe ()
  ((top-character
    :documentation "The recipe's top character, or NIL."
    :initform nil
    :initarg :top-character
    :accessor top-character)
   (middle-character
    :documentation "The recipe's middle character, or NIL."
    :initform nil
    :initarg :middle-character
    :accessor middle-character)
   (bottom-character
    :documentation "The recipe's bottom character, or NIL."
    :initform nil
    :initarg :bottom-character
    :accessor bottom-character)
   (repeated-character
    :documentation "The recipe's repeated character."
    :initarg :repeated-character
    :accessor repeated-character))
  (:documentation "The Extension Recipe class.
This class represents decoded information for extensible characters. Within
the context of this library, the expression \"extension recipe\" denotes an
instance of this class."))

(defmethod print-object ((extension-recipe extension-recipe) stream)
  "Print EXTENSION-RECIPE unreadably with its repeated character to STREAM."
  (print-unreadable-object (extension-recipe stream :type t)
    (princ (repeated-character extension-recipe) stream)))

;; #### NOTE: we don't bother to check that the repeated character is not NIL
;; because this class is not exported and I trust my code.
(defun make-extension-recipe
    (repeated-character
     &rest initargs &key top-character middle-character bottom-character)
  "Make a new EXTENSION-RECIPE with REPEATED-CHARACTER and return it.
The recipe may also have a TOP-, MIDDLE-, and BOTTOM-CHARACTER."
  (declare (ignore top-character middle-character bottom-character))
  (apply #'make-instance 'extension-recipe
	 :repeated-character repeated-character initargs))



;; ==========================================================================
;; Character Metrics
;; ==========================================================================

(defclass character-metrics ()
  ((code
    :documentation "The character's numerical code."
    :initarg :code
    :reader code)
   (font
    :documentation "The character's font."
    :initarg :font
    :reader font)
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
    :documentation "The character's extension recipe, or NIL.
This slot is non-null only if this character is extensible (see TeX: the
Program [544]). It is mutually exclusive with the NEXT-CHARACTER slot, and
also with the existence of a ligature or kerning program for this character."
    :initform nil
    :accessor extension-recipe))
  (:documentation "The Character Metrics class.
This class represents decoded character information. Within the context of
this library, the term \"character\" denotes an instance of this class."))

(defmethod print-object ((character character-metrics) stream)
  "Print CHARACTER unreadably with its code to STREAM."
  (print-unreadable-object (character stream :type t)
    (princ (code character) stream)))

(defun make-character-metrics (code font width height depth italic-correction)
  "Make a new CHARACTER-METRICS instance, and return it.
Initialize the character's CODE, FONT, WIDTH, HEIGHT, DEPTH, and
ITALIC-CORRECTION appropriately. The two remaining slots (NEXT-CHARACTER and
EXTENSION-RECIPE) will be initialized later if needed, when all character
metrics instances are created."
  (make-instance 'character-metrics
    :code code
    :font font
    :width width
    :height height
    :depth depth
    :italic-correction italic-correction))


;; ---------------------------------
;; Extension Recipe Pseudo-Accessors
;; ---------------------------------

(defun extensiblep (character)
  "Return T if CHARACTER has an extension recipe."
  ;; We don't want to expose the recipe itself.
  (when (extension-recipe character) t))

(define-condition not-extensible (tfm-usage-error)
  ((value :initarg :value :accessor value))
  (:report (lambda (not-extensible stream)
	     (format stream "Character ~A is not extensible."
	       (value not-extensible))))
  (:documentation "The Not Extensible usage error.
It signals that character VALUE doesn't have an extension recipe.
This error is not restartable."))

(defmacro define-extension-recipe-pseudo-accessor (name)
  `(defmethod ,name ((character character-metrics))
     (unless (extensiblep character) (error 'not-extensible :value character))
     (,name (extension-recipe character))))

(define-extension-recipe-pseudo-accessor top-character)
(define-extension-recipe-pseudo-accessor middle-character)
(define-extension-recipe-pseudo-accessor bottom-character)
(define-extension-recipe-pseudo-accessor repeated-character)

;;; character.lisp ends here
