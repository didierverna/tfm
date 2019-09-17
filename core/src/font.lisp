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
;; Font
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
   (face
    :documentation "The font's face number.
Note that meaningful values are numbers. A value of NIL means that the
information in unavailable."
    :initform nil
    :accessor face)
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
   ;; #### WARNING: would it make sense to also have a hash by pairs of
   ;; character /codes/?
   (ligatures
    :documentation "The font's ligatures.
This is a hash table associating conses of characters with the corresponding
ligature."
    :initform (make-hash-table :test #'equal)
    :accessor ligatures)
   ;; #### WARNING: would it make sense to also have a hash by pairs of
   ;; character /codes/?
   (kernings
    :documentation "The font's kernings.
This is a hash table associating conses of characters with the corresponding
kerning, in design size units."
    :initform (make-hash-table :test #'equal)
    :accessor kernings)
   (right-boundary-character
    :documentation "The font's right boundary character, if any.
This character is also accessible by code, like normal ones. However, it is
the only character the code of which may be outside [MIN-CODE,MAX-CODE] (see
TeX: the Program [545]). Finally, this character is not included in the
character count, unless it exists for real in the font."
    :initform nil
    :accessor right-boundary-character))
  (:documentation "The TeX Font Metrics class.
This class represents decoded font information. Within the context of this
library, the term \"font\" denotes an instance of this class."))

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

(defun character-by-code (code font &optional errorp)
  "Return FONT's character with CODE.
If ERRORP, signal an error if not found."
  (or (gethash code (characters font))
      (when errorp (error "Character code ~A not found in ~S." code font))))

(defun (setf character-by-code) (character font)
  "Make FONT's CHARACTER accessible by its code."
  (setf (gethash (code character) (characters font)) character))

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

;;; font.lisp ends here
