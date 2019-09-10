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
    :documentation "The character's italic correction, in design size units."
    :initarg :italic-correction
    :reader italic-correction)
   ;; #### WARNING: for now, the handling of character lists is very basic: I
   ;; only store the "next character" here, exactly as it is provided in the
   ;; TFM file. There's no specific representation of character lists and no
   ;; checking for cycles for instance. I'm not sure whether storing the next
   ;; character here is the right way to do it (as opposed to creating
   ;; character lists in the font instance directly). I guess I'll have to
   ;; wait until it's actually used to figure this out.
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
   ;; opposed to, for example, in the font instance directly). I guess I'll
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
This class represents decoded character information. Within the context of
this library, the term \"character\" denotes an instance of this class."))

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



;; ==========================================================================
;; Extension Recipe Pseudo-Accessors
;; ==========================================================================

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

;;; character-metrics.lisp ends here
