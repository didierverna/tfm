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


(define-condition stream-mixin ()
  ((stream :initarg :stream :accessor tfm-stream))
  (:documentation "The Stream Mixin condition.
It is used in all condition signalled while reading a STREAM."))

(defun report
    (stream-mixin stream
     &aux (tfm-stream (tfm-stream stream-mixin))
	  (pathname (when (typep tfm-stream 'file-stream)
		      (pathname tfm-stream))))
  "Report STREAM-MIXIN on STREAM.
This function is used to print the start of STREAM-MIXIN condition reports.
It writes \"While reading ..., \" on STREAM, arranging to print the stream's
file name instead of the stream itself, for conditions associated with file
streams."
  (format stream "While reading ~A, " (or pathname tfm-stream)))

#i(stream-report 2)
(defun stream-report (stream condition format-string &rest format-arguments)
  "Report a stream CONDITION with FORMAT-STRING and FORMAT-ARGUMENTS on STREAM.
Before calling FORMAT, call REPORT with CONDITION and STREAM."
  (report condition stream)
  (apply #'format stream format-string format-arguments))


(define-condition tfm-compliance-warning (stream-mixin tfm-warning)
  ()
  (:documentation "The TFM  compliance warnings root condition.
This is the root condition for warnings related to TFM compliance.
It uses STREAM-MIXIN because all compliance checks are performed when a font
is loaded."))

(define-condition tfm-compliance-error (stream-mixin tfm-error)
  ()
  (:documentation "The TFM compliance errors root condition.
This is the root condition for errors related to TFM compliance.
It uses STREAM-MIXIN because all compliance checks are performed when a font
is loaded."))



;; ==========================================================================
;; Numerical Values
;; ==========================================================================

(define-condition u16 (tfm-compliance-error)
  ((value :initarg :value :accessor value))
  (:report (lambda (u16 stream)
	     (stream-report stream u16
	       "unsigned 16 bits integer ~A is greater than 2^15."
	       (value u16))))
  (:documentation "The U16 error.
It signals that an unsigned 16 bits integer VALUE is greater than 2^15."))


(defun read-u16 (stream &optional limit)
  "Read an unsigned 16 bits Big Endian integer from STREAM.
If LIMIT, check that the integer is less than 2^15."
  (let ((u16 0))
    (setf (ldb (byte 8 8) u16) (read-byte stream)
	  (ldb (byte 8 0) u16) (read-byte stream))
    (when (and limit (not (zerop (ldb (byte 1 15) u16))))
      (restart-case (error 'u16 :value u16 :stream stream)
	(use-zero () :report "Use zero instead." (setq u16 0))))
    u16))


(defun read-u32 (stream)
  "Read an unsigned 32 bits Big Endian integer from STREAM."
  (let ((u32 0))
    (setf (ldb (byte 8 24) u32) (read-byte stream)
	  (ldb (byte 8 16) u32) (read-byte stream)
	  (ldb (byte 8  8) u32) (read-byte stream)
	  (ldb (byte 8  0) u32) (read-byte stream))
    u32))


(define-condition fix-word (tfm-compliance-error)
  ((value :initarg :value :accessor value))
  (:report (lambda (fix-word stream)
	     (stream-report stream fix-word
	       "fix word ~A is outside ]-16,+16[." (value fix-word))))
  (:documentation "The Fix Word error.
It signals that a fix word VALUE is outside ]-16,+16[."))

(defun read-fix-word (stream &optional limit)
  "Read a fix word from STREAM.
If LIMIT, check that the number lies within ]-16,+16[."
  (let* ((bytes (read-u32 stream))
	 (neg (= (ldb (byte 1 31) bytes) 1))
	 fix-word)
    (when neg (setq bytes (lognot (1- bytes))))
    (setq fix-word (+ (* (ldb (byte 8 24) bytes) (expt 2 4))
		      (* (ldb (byte 8 16) bytes) (expt 2 -4))
		      (* (ldb (byte 8  8) bytes) (expt 2 -12))
		      (* (ldb (byte 8  0) bytes) (expt 2 -20))))
    (when neg (setq fix-word (- fix-word)))
    (when (and limit (not (< -16 fix-word 16)))
      (restart-case (error 'fix-word :value fix-word :stream stream)
	(use-zero () :report "Use zero instead." (setq fix-word 0))))
    fix-word))



;; ==========================================================================
;; Strings
;; ==========================================================================

(defun read-padded-string
    (stream padding
     &aux (length (read-byte stream)) (string (make-string length)))
  "Read a string out of PADDING bytes from STREAM.
The first byte in STREAM indicates the actual length of the string.
The remaining bytes are ignored."
  (loop :for i :from 0 :upto (1- length)
	;; #### NOTE: this assumes that Lisp's internal character encoding
	;; agrees at least with ASCII.
	:do (setf (aref string i) (code-char (read-byte stream))))

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
  (loop :repeat (- padding 1 length) :do (read-byte stream))
  string)

;;; util.lisp ends here
