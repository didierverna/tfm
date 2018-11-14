;;; tfm.lisp --- TeX Font Metrics

;; Copyright (C) 2018 Didier Verna

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

(in-package :cl-user)

(defpackage :net.didierverna.tfm
  (:documentation "The TeX Font Metrics package.")
  (:use :cl)
  (:export
    :nickname-package

    ;; Entry point:
    :parse))

(in-package :net.didierverna.tfm)


;; ==========================================================================
;; Utilities
;; ==========================================================================

;; --------
;; External
;; --------

(defun nickname-package (&optional (nickname :tfm))
  "Add NICKNAME (:TFM by default) to the :NET.DIDIERVERNA.TFM package."
  (rename-package :net.didierverna.tfm
		  (package-name :net.didierverna.tfm)
		  (adjoin nickname (package-nicknames :net.didierverna.tfm)
			  :test #'string-equal)))


;; --------
;; Internal
;; --------

(defun read-u16 (stream &optional limit)
  "Read an unsigned 16 bits Big Endian integer from STREAM.
If LIMIT, check that the integer is less than 2^15."
  (let ((u16 0))
    (setf (ldb (byte 8 8) u16) (read-byte stream)
	  (ldb (byte 8 0) u16) (read-byte stream))
    (when (and limit (not (zerop (ldb (byte 1 15) u16))))
      (error "Unsigned 16bits integer too large (>= 2^15): ~A." u16))
    u16))

(defun read-u32 (stream)
  "Read an unsigned 32 bits Big Endian integer from STREAM."
  (let ((u32 0))
    (setf (ldb (byte 8 24) u32) (read-byte stream)
	  (ldb (byte 8 16) u32) (read-byte stream)
	  (ldb (byte 8  8) u32) (read-byte stream)
	  (ldb (byte 8  0) u32) (read-byte stream))
    u32))

(defun read-fix (stream &optional limit)
  "Read a fix word from STREAM.
If LIMIT, check that the number lies within [-16,16]."
  (let* ((bytes (read-u32 stream))
	 (neg (= (ldb (byte 1 31) bytes) 1))
	 value)
    (when neg (setq bytes (lognot (1- bytes))))
    (setq value (+ (* (ldb (byte 8 24) bytes) (expt 2 4))
		   (* (ldb (byte 8 16) bytes) (expt 2 -4))
		   (* (ldb (byte 8  8) bytes) (expt 2 -12))
		   (* (ldb (byte 8  0) bytes) (expt 2 -20))))
    (when neg (setq value (- value)))
    ;; #### FIXME: <= or < ?
    (when (and limit (not (<= -16 value 16)))
      (error "Fix word outside [-16,16] range: ~A." value))
    value))



;; ==========================================================================
;; TeX Font Metrics
;; ==========================================================================

;; -----
;; Class
;; -----

(defclass tfm ()
  ((name :initarg :name :reader name)
   (checksum :accessor checksum)
   (design-size :accessor design-size)
   (min-code :accessor min-code)
   (max-code :accessor max-code))
  (:documentation "The TeX Font Metrics class."))

(defmethod print-object ((tfm tfm) stream)
  "Print TFM unreadably with its font name to STREAM."
  (print-unreadable-object (tfm stream :type t)
    (princ (name tfm) stream)))



;; ==========================================================================
;; Header
;; ==========================================================================

(defun parse-header (stream length tfm)
  "Parse a TFM hedaer of LENGTH in STREAM."
  ;; #### WARNING: comparative tests with tftopl show a different checkum, but
  ;; the checkum I see doesn't fit 32 bits, so I don't understand what they're
  ;; doing...
  (setf (checksum tfm) (read-u32 stream))
  (setf (design-size tfm) (read-fix stream))
  (if (< (design-size tfm) 1)
    (error "Design size should be >= 1: ~A" (design-size tfm)))
  ;; #### NOTE: FILE-POSITION maybe?
  (loop :repeat (- length 2) :do (read-u32 stream)))



;; ==========================================================================
;; Entry Point
;; ==========================================================================

(defun parse (file &aux (tfm (make-instance 'tfm :name (pathname-name file))))
  "Parse TFM FILE into a TFM instance. Return that instance."
  (with-open-file (stream file
		   :direction :input :element-type '(unsigned-byte 8))
    (let ((lf (read-u16 stream t))
	  (lh (read-u16 stream t))
	  (bc (read-u16 stream t))
	  (ec (read-u16 stream t))
	  (nw (read-u16 stream t))
	  (nh (read-u16 stream t))
	  (nd (read-u16 stream t))
	  (ni (read-u16 stream t))
	  (nl (read-u16 stream t))
	  (nk (read-u16 stream t))
	  (ne (read-u16 stream t))
	  (np (read-u16 stream t)))
      (unless (and (<= (1- bc) ec) (<= ec 255))
	(error "Invalid smallest / largest character codes: ~A / ~A." bc ec))
      (when (> bc 255)
	(setq bc 1 ec 0))
      (setf (min-code tfm) bc
	    (max-code tfm) ec)
      (unless (= lf (+ 6 lh (+ ec (- bc) 1) nw nh nd ni nl nk ne np))
	(error "Invalid lengths in file header."))
      (unless (= (file-length stream) (* 4 lf))
	(error "File length doesn't match header."))
      (when (or (zerop nw) (zerop nh) (zerop nd) (zerop ni))
	(error "Empty width, height, depth or italic correction table."))
      (unless (>= lh 2)
	(error "Invalid header length: too small."))
      (parse-header stream lh tfm)
      ))
  tfm)

;; tfm.lisp ends here
