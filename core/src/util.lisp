;;; util.lisp --- General Utilities

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
;; Error Ontology
;; ==========================================================================

(define-condition tfm ()
  ()
  (:documentation "The TFM root condition."))


(define-condition tfm-warning (tfm warning)
  ()
  (:documentation "The TFM warnings root condition."))

(define-condition tfm-error (tfm error)
  ()
  (:documentation "The TFM errors root condition."))


(define-condition tfm-compliance (tfm)
  ()
  (:documentation "The TFM Compliance root condition.
This is the mixin for conditions related to TFM compliance."))

(define-condition tfm-compliance-warning (tfm-warning tfm-compliance)
  ()
  (:documentation "The TFM  compliance warnings root condition.
This is the root condition for warnings related to TFM compliance."))

(define-condition tfm-compliance-error (tfm-error tfm-compliance)
  ()
  (:documentation "The TFM compliance errors root condition.
This is the root condition for errors related to TFM compliance."))


(define-condition tfm-usage (tfm)
  ()
  (:documentation "The TFM Usage root condition.
This is the mixin for conditions related to the use of the library."))

(define-condition tfm-usage-warning (tfm-warning tfm-usage)
  ()
  (:documentation "The TFM usage warnings root condition.
This is the root condition for warnings related to the use of the library."))

(define-condition tfm-usage-error (tfm-error tfm-usage)
  ()
  (:documentation "The TFM usage errors root condition.
This is the root condition for errors related to the use of the library."))



;; ==========================================================================
;; Stream Reading
;; ==========================================================================

(defvar *stream* nil "The stream being read.")

#i(report 2)
(defun report (stream format-string &rest format-arguments)
  "Like FORMAT, but if *STREAM* is bound, report that we're reading from it."
  (if *stream*
    (format stream "While reading ~A, "
      (or (when (typep *stream* 'file-stream) (pathname *stream*))
	  *stream*))
    (when (alpha-char-p (aref format-string 0))
      (setf (aref format-string 0) (char-upcase (aref format-string 0)))))
  (apply #'format stream format-string format-arguments))


;; ----------------
;; Numerical values
;; ----------------

(define-condition u16-overflow (tfm-compliance-error)
  ((value :documentation "The faulty value." :initarg :value :accessor value))
  (:report (lambda (u16-overflow stream)
	     (report stream "unsigned 16 bits integer ~A is greater than 2^15."
	       (value u16-overflow))))
  (:documentation "The U16 Overflow compliance error.
It signals that an unsigned 16 bits integer is greater than 2^15."))

(defun read-u16 ()
  "Read an unsigned 16 bits Big Endian integer from *STREAM* and return it.
If >= 2^15, signal a U16-OVERFLOW error."
  (let ((u16 0))
    (setf (ldb (byte 8 8) u16) (read-byte *stream*)
	  (ldb (byte 8 0) u16) (read-byte *stream*))
    (unless (zerop (ldb (byte 1 15) u16))
      (error 'u16-overflow :value u16))
    u16))


(defun read-u32 ()
  "Read an unsigned 32 bits Big Endian integer from *STREAM*."
  (let ((u32 0))
    (setf (ldb (byte 8 24) u32) (read-byte *stream*)
	  (ldb (byte 8 16) u32) (read-byte *stream*)
	  (ldb (byte 8  8) u32) (read-byte *stream*)
	  (ldb (byte 8  0) u32) (read-byte *stream*))
    u32))


(define-condition fix-word-overflow (tfm-compliance-error)
  ((value :documentation "The faulty value." :initarg :value :accessor value))
  (:report (lambda (fix-word-overflow stream)
	     (report stream "fix word ~A is outside ]-16,+16[."
	       (value fix-word-overflow))))
  (:documentation "The Fix Word Overflow compliance error.
It signals that a fix word is outside ]-16,+16[."))

(defun read-fix-word (&optional (limit t))
  "Read a fix word from *STREAM* and return it.
If LIMIT (the default), check that the number lies within ]-16,+16[, or
signal a FIX-WORD-OVERFLOW error. This error is immediately restartable with
SET-TO-ZERO."
  (let* ((bytes (read-u32))
	 (neg (= (ldb (byte 1 31) bytes) 1))
	 fix-word)
    (when neg (setq bytes (lognot (1- bytes))))
    (setq fix-word (+ (* (ldb (byte 8 24) bytes) (expt 2 4))
		      (* (ldb (byte 8 16) bytes) (expt 2 -4))
		      (* (ldb (byte 8  8) bytes) (expt 2 -12))
		      (* (ldb (byte 8  0) bytes) (expt 2 -20))))
    (when neg (setq fix-word (- fix-word)))
    (when (and limit (not (< -16 fix-word 16)))
      (restart-case (error 'fix-word-overflow :value fix-word)
	(set-to-zero () :report "Set it to 0." (setq fix-word 0))))
    fix-word))


;; -------
;; Strings
;; -------

(define-condition invalid-string-length (tfm-compliance-error)
  ((value
    :documentation "The invalid length."
    :initarg :value
    :accessor value)
   (padding
    :documentation "The maximum length."
    :initarg :padding
    :accessor padding))
  (:report (lambda (invalid-string-length stream)
	     (report stream "~
declared padded string length ~A is greater than its maximum ~A."
	       (value invalid-string-length)
	       (1- (padding invalid-string-length)))))
  (:documentation "The Invalid String Length compliance error.
It signals that the declared length of a padded string is greater than its
maximum."))

(define-condition invalid-bcpl-string (tfm-compliance-error)
  ((value :documentation "The invalid string." :initarg :value :accessor value))
  (:report (lambda (invalid-bcpl-string stream)
	     (report stream "~
BCPL string ~S is invalid (it shouldn't contain parentheses or non-ASCII ~
characters)."
	       (value invalid-bcpl-string))))
  (:documentation "The Invalid BCPL String compliance error.
It signals that a BCPL string contains parentheses or non-ASCII characters."))

(defun read-padded-string
    (padding &aux (length (read-byte *stream*)) string)
  "Read a BCPL string out of PADDING bytes from *STREAM*.
The first byte in *STREAM* indicates the actual length of the string.
The remaining bytes are ignored.

If the declared string length is too large, signal an INVALID-STRING-LENGTH
error. This error is immediately restartable with READ-MAXIMUM-LENGTH or
DISCARD-STRING.

If the string is not in BCPL format (it contains parentheses or non plain
ASCII characters, signal an INVALID-BCPL-STRING error. This error is
immediately restartable with KEEP-STRING, FIX-STRING (replacing parentheses
with slashes, and non plain ASCII characters with question marks), or
DISCARD-STRING."
  (unless (< length padding)
    (restart-case (error 'invalid-string-length :value length :padding padding)
      (read-maximum-length () :report "Read the maximum possible length."
	(setq length (1- padding)))
      (discard-string () :report "Discard the string."
	(setq length 0))))
  (unless (zerop length)
    (setq string (make-string length))
    (loop :for i :from 0 :upto (1- length)
	  ;; #### NOTE: this assumes that Lisp's internal character encoding
	  ;; agrees at least with ASCII.
	  :do (setf (aref string i) (code-char (read-byte *stream*))))
    (when (or (find #\( string)
	      (find #\) string)
	      (find-if (lambda (character)
			 (or (< (char-code character) 32)
			     (> (char-code character) 126)))
		       string))
      (restart-case (error 'invalid-bcpl-string :value string)
	(keep-string () :report "Keep it anyway.")
	(fix-string () :report "Fix it using /'s and ?'s."
	  (loop :for i :from 0 :upto (1- (length string))
		:do (cond ((or (char= (aref string i) #\()
			       (char= (aref string i) #\)))
			   (setf (aref string i) #\/))
			  ((or (< (char-code (aref string i)) 32)
			       (> (char-code (aref string i)) 126))
			   (setf (aref string i) #\?)))))
	(discard-string () :report "Discard it."
	  (setq string nil)))))

  ;; #### NOTE: David Fuchs'paper in TUGboat Vol.2 n.1 says that the remaining
  ;; bytes should be 0, but this doesn't appear to be always the case. For
  ;; example, the pagd8y.tfm file has a "Y&Y Inc" string hidden in the
  ;; character coding scheme string's remainder.

  ;; Here's a comment from Doug McKenna about this: So the answer as to
  ;; whether the remaining space is required to be nulls should be in the
  ;; source code of pltotf. And indeed, that's what it does, although the
  ;; notes in the source code say the nullification of the remaining garbage
  ;; bytes wasn't added until two years later, in April 1983 (Version 1.3).
  ;; Search the WEB source code for "tidy up the remaining bytes", which is
  ;; commenting on the routine creating a BCPL string.

  ;; So it may very well be the case that older tfm files do have garbage
  ;; after the actual string.
  (loop :repeat (- padding 1 length) :do (read-byte *stream*))
  string)



;; ==========================================================================
;; Miscellaneous
;; ==========================================================================

(defmacro define-constant (name value &optional documentation)
  "Like DEFCONSTANT, but reuse existing value if any."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when documentation (list documentation))))

(defmacro map-accessors (var object accessors &body body)
  "Map BODY on OBJECT ACCESSORS, each in turn available as VAR."
  `(progn ,@(mapcar (lambda (accessor)
		      `(with-accessors ((,var ,accessor)) ,object
			   ,@body))
	      accessors)))

;;; util.lisp ends here
