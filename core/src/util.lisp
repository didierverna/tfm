;;; util.lisp --- General Utilities

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


(defvar *stream* nil "The stream being read.")


;; ==========================================================================
;; Context-Aware Condition Reporting and Handling
;; ==========================================================================

(defclass context ()
  ()
  (:documentation "The CONTEXT class.
This is the base class for classes representing contexts in which
conditions are signalled."))

(defgeneric context-string (context)
  (:documentation "Return CONTEXT'string."))

(defun context-format
    (stream context format-string &rest arguments &aux (upcase t))
  "Like FORMAT, but *STREAM* and CONTEXT-aware.
- When *STREAM*, report that we're reading from it to STREAM.
- When CONTEXT, report the context string to STREAM.
- Finally, print FORMAT-STRING with ARGUMENTS to STREAM."
  (when *stream*
    (format stream "While reading ~A,~%"
      (if (typep *stream* 'file-stream) (pathname *stream*) *stream*))
    (setq upcase nil))
  (when context
    (format stream "~:[~A~;~@(~A~)~],~%" upcase (context-string context))
    (setq upcase nil))
  (apply #'format stream "~:[~@?~;~@(~@?~)~]." upcase format-string arguments))

(defmacro define-condition-report
    ((condition type) format-string &rest arguments)
  "Define a context-aware report function for a CONDITION of TYPE.
The reporting is ultimately done by calling FORMAT on FORMAT-STRING with
ARGUMENTS."
  (let ((the-stream (gensym "stream")))
    `(defmethod print-object ((,condition ,type) ,the-stream)
       (if *print-escape*
	 (call-next-method)
	 (context-format ,the-stream (context ,condition)
	   ,format-string ,@arguments)))))

(defmacro with-condition-context
    ((condition-type context-type &rest initargs) &body body)
  "Execute BODY within a particular condition signalling context.
While BODY is executing, conditions of type CONDITION-TYPE (not evaluated) are
caught and augmented with an instance of CONTEXT-TYPE (not evaluated)
initialized with INITARGS."
  `(handler-bind ((,condition-type
		    (lambda (condition)
		      (setf (slot-value condition 'context)
			    (make-instance ',context-type ,@initargs)))))
     ,@body))




;; ==========================================================================
;; Error Ontology
;; ==========================================================================

(define-condition tfm ()
  ((context :documentation "The context in which the condition was signalled."
	    :initform nil :reader context))
  (:documentation "The TFM root condition."))


(define-condition tfm-warning (tfm warning)
  ()
  (:documentation "The TFM warnings root condition."))

(define-condition tfm-error (tfm error)
  ()
  (:documentation "The TFM errors root condition."))


(define-condition tfm-compliance (tfm)
  ((section :documentation "The related TFtoPL section."
	    :allocation :class :initform nil :reader section))
  (:documentation "The TFM Compliance root condition.
This is the mixin for conditions related to TFM compliance."))

(defmethod print-object :after ((condition tfm-compliance) stream)
  "Advertise CONDITION's relevant TFtoPL section."
  (unless (or *print-escape* (not (section condition)))
    (format stream
	"~&See §~A of TFtoPL for more information."
      (section condition))))

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
;; Numbers
;; ==========================================================================

(defun read-u8 ()
  "Read an unsigned 8 bits integer from *STREAM*."
  (read-byte *stream*))


(define-condition u16-overflow (tfm-compliance-error)
  ((section :initform 8) ; slot merge
   (value :documentation "The faulty value." :initarg :value :reader value))
  (:documentation "The U16 Overflow compliance error.
It signals that an unsigned 16 bits integer is greater than 2^15."))

(define-condition-report (condition u16-overflow)
  "unsigned 16 bits integer ~A is greater than 2^15"
  (value condition))

(defun read-u16 ()
  "Read an unsigned 16 bits Big Endian integer from *STREAM* and return it.
If >= 2^15, signal a U16-OVERFLOW error."
  (let ((u16 0))
    (setf (ldb (byte 8 8) u16) (read-u8)
	  (ldb (byte 8 0) u16) (read-u8))
    (unless (zerop (ldb (byte 1 15) u16))
      (error 'u16-overflow :value u16))
    u16))


(defun read-u32 ()
  "Read an unsigned 32 bits Big Endian integer from *STREAM*."
  (let ((u32 0))
    (setf (ldb (byte 8 24) u32) (read-u8)
	  (ldb (byte 8 16) u32) (read-u8)
	  (ldb (byte 8  8) u32) (read-u8)
	  (ldb (byte 8  0) u32) (read-u8))
    u32))


(define-condition fix-word-overflow (tfm-compliance-error)
  ((section :initform 9) ; slot merge
   (value :documentation "The faulty value." :initarg :value :reader value))
  (:documentation "The Fix Word Overflow compliance error.
It signals that a fix word is outside ]-16,+16[."))

(define-condition-report (condition fix-word-overflow)
  "fix word ~A (~A) is outside ]-16,+16["
  (value condition)
  (float (value condition)))

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




;; ==========================================================================
;; Padded Strings
;; ==========================================================================

;; #### WARNING: I'm lucky enough that padded strings only occur in the header
;; section, and hence relate to section 10 of TFtoPL. Otherwise, I'd be in
;; trouble with the class-wide SECTION slot in conditions...

;; #### NOTE: this mixin is currently empty but serves as a catchall for
;; WITH-CONDITION-CONTEXT.
(define-condition padded-string (tfm)
  ()
  (:documentation "The Padded String condition.
This is a mixin for all conditions related to padded strings."))


(define-condition invalid-padded-string-length
    (tfm-compliance-error padded-string)
  ((section :initform 10) ; slot merge
   (value
    :documentation "The invalid length."
    :initarg :value
    :reader value)
   (pad
    :documentation "The maximum length."
    :initarg :pad
    :reader pad))
  (:documentation "The Invalid Padded String Length compliance error.
It signals that the declared length of a padded string is greater than its
maximum."))

(define-condition-report (condition invalid-padded-string-length)
  "declared padded string length ~A is greater than its maximum ~A"
  (value condition)
  (1- (pad condition)))


(define-condition invalid-padded-string (tfm-compliance-error padded-string)
  ((section :initform 10) ; slot merge
   (value :documentation "The invalid string." :initarg :value :reader value))
  (:documentation "The Invalid Padded String compliance error.
It signals that a padded string is not in BCPL format (it contains parentheses
or non-ASCII characters)."))

(define-condition-report (condition invalid-padded-string)
  "padded string ~S is not in BCPL format (it contains parentheses and/or ~
non-ASCII characters)"
  (value condition))


(define-condition padded-string-overflow (tfm-compliance-warning padded-string)
  ((value :documentation "The string's overflow."
	  :initarg :value :reader value))
  (:documentation "The Padded String Overflow compliance warning.
It signals that a padded string contains non null characters after its
declared length."))

(define-condition-report (condition padded-string-overflow)
  "padded string contains non-null overflow characters (~S)"
  (value condition))

(defmethod print-object :after ((condition padded-string-overflow) stream)
  "Advertise padded string overflow CONDITION's relevant documentation."
  (unless *print-escape*
    (format stream
	"~&This font may have been created before April 1983.
See §87 of the PLtoTF documentation, or “TeX Font Metrics Files”
(David Fuchs, TUGBoat, Volume 2, №1) for more information.")))


(defun read-padded-string
    (pad &aux (length (read-u8)) string)
  "Read a padded string out of PAD bytes from *STREAM*.
The first byte in *STREAM* indicates the actual length of the string.

If the declared string's length is too large, signal an
INVALID-PADDED-STRING-LENGTH error. This error is immediately restartable with
READ-MAXIMUM-LENGTH or DISCARD-STRING.

If the string is not in BCPL format (it contains parentheses or non-ASCII
characters, signal an INVALID-PADDED-STRING error. This error is immediately
restartable with KEEP-STRING, FIX-STRING (replacing parentheses with slashes,
and non-ASCII characters with question marks), or DISCARD-STRING.

If the string is not padded with zeros, signal a PADDED-STRING-OVERFLOW
warning."
  (unless (< length pad)
    (restart-case (error 'invalid-padded-string-length :value length :pad pad)
      (read-maximum-length () :report "Read the maximum possible length."
	(setq length (1- pad)))
      (discard-string () :report "Discard the string."
	(return-from read-padded-string)))) ; also bypass the overflow check
  (unless (zerop length)
    (setq string (make-string length))
    (loop :for i :from 0 :upto (1- length)
	  ;; #### NOTE: this assumes that Lisp's internal character encoding
	  ;; agrees at least with ASCII.
	  :do (setf (aref string i) (code-char (read-u8))))
    (when (or (find #\( string)
	      (find #\) string)
	      (find-if (lambda (character)
			 (or (< (char-code character) 32)
			     (> (char-code character) 126)))
		       string))
      (restart-case (error 'invalid-padded-string :value string)
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
  ;; after the actual string. In any case, this is worth a warning.
  (let ((tail-length (- pad 1 length)))
    (unless (zerop tail-length)
      (let ((tail (make-string tail-length)))
	(loop :for i :from 0 :upto (1- tail-length)
	      ;; #### NOTE: this assumes that Lisp's internal character
	      ;; encoding agrees at least with ASCII.
	      :do (setf (aref tail i) (code-char (read-u8))))
	(when (find-if-not #'zerop tail :key #'char-code)
	  (warn 'padded-string-overflow :value tail)))))
  string)




;; ==========================================================================
;; Miscellaneous
;; ==========================================================================

(defmacro define-constant (name value &optional documentation)
  "Like DEFCONSTANT, but reuse existing value if any."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when documentation (list documentation))))

(defmacro map-slots (var object slots &body body)
  "Map BODY on OBJECT SLOTS, each in turn available as VAR."
  `(progn ,@(mapcar (lambda (slot)
		      `(with-slots ((,var ,slot)) ,object ,@body))
	      slots)))

;;; util.lisp ends here
