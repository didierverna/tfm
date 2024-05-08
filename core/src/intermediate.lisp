;;; intermediate.lisp --- Low level, intermediate data structures

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

;; This file contains low level, intermediate data structures that closely
;; match the contents of TFM files and need decoding. Because of that, we
;; choose to keep the TeX terminology.


;;; Code:

(in-package :net.didierverna.tfm)
(in-readtable :net.didierverna.tfm)


;; ==========================================================================
;; Character information
;; ==========================================================================

(defstruct (char-info :conc-name)
  "The Char Info structure.
This structure is used to store decoded information from the char-info table
(see TeX: the Program [543]). Only one of LIG/KERN-INDEX, NEXT-CHAR, and
  EXTEN-INDEX may be non-null at a time (see TeX: the Program [544])."
  width-index height-index depth-index italic-index
  lig/kern-index next-char exten-index)

(define-condition invalid-char-info (tfm-compliance-error)
  ((value
    :documentation "The invalid char-info structure."
    :initarg :value
    :accessor value))
  (:documentation "The Invalid Char Info compliance error.
It signals that a char-info with a width-index of 0 is not completely
zero'ed out."))

(define-condition-report (condition invalid-char-info)
  "~A is invalid (should be zero'ed out)"
  (value condition))

(defun decode-char-info (word)
  "Decode char-info WORD into a new CHAR-INFO instance, and return it."
  (let ((char-info (make-char-info
		    :width-index (ldb (byte 8 24) word)
		    :height-index (ldb (byte 4 20) word)
		    :depth-index (ldb (byte 4 16) word)
		    :italic-index (ldb (byte 6 10) word)))
	(tag (ldb (byte 2 8) word))
	(remainder (ldb (byte 8 0) word)))
    (case tag
      (1 (setf (lig/kern-index char-info) remainder))
      (2 (setf (next-char char-info) remainder))
      (3 (setf (exten-index char-info) remainder)))
    (unless (or (not (zerop (width-index char-info))) (zerop word))
      (restart-case (error 'invalid-char-info :value char-info)
	(set-to-zero () :report "Zero it out."
	  (setq char-info (decode-char-info 0)))))
    char-info))



;; ==========================================================================
;; Extensible Recipes
;; ==========================================================================

(defstruct (exten :conc-name)
  "The Exten structure.
This structure is used to store decoded information from the exten table
(see TeX: the Program [546])."
  top mid bot rep)

(defun decode-exten (word)
  "Decode exten WORD into a new EXTEN instance, and return it."
  (make-exten
   :top (ldb (byte 8 24) word)
   :mid (ldb (byte 8 16) word)
   :bot (ldb (byte 8  8) word)
   :rep (ldb (byte 8  0) word)))



;; ==========================================================================
;; Ligature/Kerning Instructions
;; ==========================================================================

(defstruct (lig/kern :conc-name)
  "The Lig/Kern structure.
This structure is used to store decoded information from the lig/kern table
(see TeX: the Program [545])."
  skip next op remainder)

(defun decode-lig/kern (word)
  "Decode lig/kern WORD into a new LIG/KERN instance, and return it."
  (make-lig/kern
   :skip (ldb (byte 8 24) word)
   :next (ldb (byte 8 16) word)
   :op (ldb (byte 8 8) word)
   :remainder (ldb (byte 8 0) word)))

;;; intermediate.lisp ends here
