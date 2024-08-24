;;; intermediate.lisp --- Low level, intermediate data structures

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

;; #### FIXME: this one is a bit obscure and I don't fully understand it.
;; There seems to be fonts that are all zero'ed out, except for a lig/kern
;; program index. Why would a non-existent character in the font still have a
;; lig/kern program ? One possibility would be a bounding character (in which
;; case this warning would be wrong), but experimentation shows that it's not.


(define-condition spurious-char-info (tfm-compliance-warning)
  ((section :initform 11) ; slot merge
   (value
    :documentation "The culprit char-info structure."
    :initarg :value
    :accessor value))
  (:documentation "The Spurious Char Info compliance warning.
It signals that a char-info for a non-existent character (that is, with a
width-index of 0) is not completely zero'ed out."))

(define-condition-report (condition spurious-char-info)
  "char-info structure for a non-existent character is not blank~%~A"
  (value condition))


(defun read-char-info ()
  "Read one char-info from *STREAM* into a new CHAR-INFO instance.
If the char-info denotes a non-existent character (that is, it is has a width
index of 0) but is not completely blank, signal a SPURIOUS-CHAR-INFO warning."
  (let* ((w (read-u8))
	 (h&d (read-u8))
	 (i&t (read-u8))
	 (char-info (make-char-info
		     :width-index w
		     :height-index (ldb (byte 4 4) h&d)
		     :depth-index (ldb (byte 4 0) h&d)
		     :italic-index (ldb (byte 6 2) i&t)))
	 (tag (ldb (byte 2 0) i&t))
	 (remainder (read-u8)))
    (case tag
      (1 (setf (lig/kern-index char-info) remainder))
      (2 (setf (next-char char-info) remainder))
      (3 (setf (exten-index char-info) remainder)))
    (unless (or (not (zerop (width-index char-info)))
		(and (zerop h&d)
		     (zerop i&t)
		     (zerop remainder)))
      (warn 'spurious-char-info :value char-info))
    char-info))

(defun read-l0-omega-char-info ()
  "Read one char-info from *STREAM* into a new CHAR-INFO instance.
If the char-info denotes a non-existent character (that is, it is has a width
index of 0) but is not completely blank, signal a SPURIOUS-CHAR-INFO warning."
  (let* ((char-info (make-char-info
		     :width-index (read-u16 nil)
		     :height-index (read-u8)
		     :depth-index (read-u8)
		     :italic-index (read-u8)))
	 (tag (ldb (byte 2 0) (read-u8)))
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



;; ==========================================================================
;; Extensible Recipes
;; ==========================================================================

(defstruct (exten :conc-name)
  "The Exten structure.
This structure is used to store decoded information from the exten table
(see TeX: the Program [546])."
  top mid bot rep)

(defun read-exten ()
  "Read one exten from *STREAM* into a new EXTEN instance."
  (make-exten
   :top (read-u8)
   :mid (read-u8)
   :bot (read-u8)
   :rep (read-u8)))

(defun read-l0-omega-exten ()
  "Read one exten from *STREAM* into a new EXTEN instance."
  (make-exten
   :top (read-u16 nil)
   :mid (read-u16 nil)
   :bot (read-u16 nil)
   :rep (read-u16 nil)))



;; ==========================================================================
;; Ligature/Kerning Instructions
;; ==========================================================================

(defstruct (lig/kern :conc-name)
  "The Lig/Kern structure.
This structure is used to store decoded information from the lig/kern table
(see TeX: the Program [545])."
  skip next op remainder)

(defun read-lig/kern ()
  "Read one lig/kern from *stream* into a new LIG/KERN instance."
  (make-lig/kern
   :skip (read-u8)
   :next (read-u8)
   :op (read-u8)
   :remainder (read-u8)))

(defun read-l0-omega-lig/kern ()
  "Read one lig/kern from *stream* into a new LIG/KERN instance."
  (make-lig/kern
   :skip (read-u16 nil)
   :next (read-u16 nil)
   :op (read-u16 nil)
   :remainder (read-u16 nil)))

;;; intermediate.lisp ends here
