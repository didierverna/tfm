;;; omega.lisp --- Omega Font Information

;; Copyright (C) 2024 Didier Verna

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


(define-condition invalid-ofm-level (tfm-compliance-error)
  ((value :documentation "The invalid level." :initarg :value :accessor value))
  (:documentation "The Invalid OFM LEVEL compliance error.
It signals that an OFM font advertises a level different from 0 or 1."))

(define-condition-report (condition invalid-ofm-level)
  "OFM level ~S is invalid (should be 0 or 1)"
  (value condition))


;; ==========================================================================
;; Level 0 Font
;; ==========================================================================

;; -----
;; Class
;; -----

(defclass l0-omega-font (font)
  ((direction :documentation "The font direction."
	      :accessor direction))
  (:documentation "The Level 0 Omega Font Metrics class."))


;; ------------
;; Intermediate
;; ------------

(defun read-l0-omega-char-info ()
  "Read one char-info from *STREAM* into a new CHAR-INFO instance.
If the char-info denotes a non-existent character (that is, it is has a width
index of 0) but is not completely blank, signal a SPURIOUS-CHAR-INFO warning."
  (let* ((char-info (make-char-info
		     :width-index (read-u16 nil)
		     :height-index (read-byte *stream*)
		     :depth-index (read-byte *stream*)
		     :italic-index (read-byte *stream*)))
	 (tag (ldb (byte 2 0) (read-byte *stream*)))
	 (remainder (read-u16 nil)))
    (case tag
      (1 (setf (lig/kern-index char-info) remainder))
      (2 (setf (next-char char-info) remainder))
      (3 (setf (exten-index char-info) remainder)))
    (unless (or (not (zerop (width-index char-info)))
		(and (zerop (height-index char-info))
		     (zerop (depth-index char-info))
		     (zerop (italic-index char-info))
		     (zerop tag)
		     (zerop remainder)))
      (warn 'spurious-char-info :value char-info))
    char-info))

;;; omega.lisp ends here

