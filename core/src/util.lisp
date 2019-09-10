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
;; Numerical Values
;; ==========================================================================

(defun read-u16 (stream &optional limit)
  "Read an unsigned 16 bits Big Endian integer from STREAM.
If LIMIT, check that the integer is less than 2^15."
  (let ((u16 0))
    (setf (ldb (byte 8 8) u16) (read-byte stream)
	  (ldb (byte 8 0) u16) (read-byte stream))
    (when (and limit (not (zerop (ldb (byte 1 15) u16))))
      (error "Unsigned 16 bits integer too large (>= 2^15): ~A." u16))
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

;;; util.lisp ends here
