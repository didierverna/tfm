;;; intermediate.lisp --- Low level, intermediate data structures

;; Copyright (C) 2018, 2019, 2024, 2025 Didier Verna

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
  ((reference :initform 11) ; slot merge
   (char-info
    :documentation "The culprit char-info structure."
    :initarg :char-info
    :reader char-info)
   (tag
    :documentation "The original tag."
    :initarg :tag
    :reader tag)
   (remainder
    :documentation "The original remainder."
    :initarg :remainder
    :reader remainder))
  (:documentation "The Spurious Char Info compliance warning.
It signals that a char-info for a non-existent character (that is, with a
width-index of 0) is not completely zero'ed out."))

;; #### NOTE: without specific knowledge of Lisp or the TFM library itself,
;; some cases of a spurious char info may be confusing to read. In particular,
;; a non-zero tag will entail a numerical value for lig/kern-index, next-char,
;; or exten-index, but if the value is 0, people may not realize that it
;; should have been NIL instead. Because of this, the condition's report will
;; give some more detail in that situation.
(define-condition-report (condition spurious-char-info)
    "char-info structure for a non-existent character is not blank~A~%~A"
  (let ((char-info (char-info condition)))
    ;; #### WARNING: EQL below because we might be comparing with NIL.
    (cond ((eql (lig/kern-index char-info) 0) " (tag = 1)")
	  ((eql (next-char char-info) 0) " (tag = 2)")
	  ((eql (exten-index char-info) 0) " (tag = 3)")
	  ((zerop (tag condition))
	   (format nil " (remainder = ~A)" (remainder condition)))
	  (t "")))
  (char-info condition))

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
      (warn 'spurious-char-info
	:char-info char-info :tag tag :remainder remainder))
    char-info))


(define-condition spurious-o0-char-info (spurious-char-info)
  ((reference :initform '(:omega . 7.1)) ; slot merge
   (tag ; slot merge, even though a bit dirty
    :documentation "The RFU/TAG byte."
    :initarg :rfu&tag
    :reader rfu&tag))
  (:documentation "The Spurious Level 0 Omega Char Info compliance warning.
It signals that a char-info for a non-existent character (that is, with a
width-index of 0) is not completely zero'ed out."))

;; See comment above the TFM counterpart.
(define-condition-report (condition spurious-o0-char-info)
  "char-info structure for a non-existent character is not blank~A~%~A"
  (let ((char-info (char-info condition)))
    ;; #### WARNING: EQL below because we might be comparing with NIL.
    (cond ((or (eql (lig/kern-index char-info) 0)
	       (eql (next-char char-info) 0)
	       (eql (exten-index char-info) 0)
	       (> (rfu&tag condition) 3))
	   (format nil " (RFU/TAG byte = 0b~B)" (rfu&tag condition)))
	  ((zerop (rfu&tag condition))
	   (format nil " (remainder = ~A)" (remainder condition)))
	  (t "")))
  (char-info condition))

(defun read-o0-char-info ()
  "Read one char-info from *STREAM* into a new CHAR-INFO instance.
If the char-info denotes a non-existent character (that is, it is has a width
index of 0) but is not completely blank, signal a SPURIOUS-CHAR-INFO warning."
  (let* ((char-info (make-char-info
		     :width-index (read-u16 nil)
		     :height-index (read-u8)
		     :depth-index (read-u8)
		     :italic-index (read-u8)))
	 (rfu&tag (read-u8))
	 (tag (ldb (byte 2 0) rfu&tag))
	 (remainder (read-u16 nil)))
    (case tag
      (1 (setf (lig/kern-index char-info) remainder))
      (2 (setf (next-char char-info) remainder))
      (3 (setf (exten-index char-info) remainder)))
    (unless (or (not (zerop (width-index char-info)))
		(and (zerop (height-index char-info))
		     (zerop (depth-index char-info))
		     (zerop (italic-index char-info))
		     (zerop rfu&tag)
		     (zerop remainder)))
      (warn 'spurious-o0-char-info
	:char-info char-info :rfu&tag rfu&tag :remainder remainder))
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

(defun read-o0-exten ()
  "Read one exten from *STREAM* into a new EXTEN instance."
  (make-exten
   :top (read-u16 nil)
   :mid (read-u16 nil)
   :bot (read-u16 nil)
   :rep (read-u16 nil)))




;; ==========================================================================
;; Ligature/Kerning Instructions
;; ==========================================================================

;; #### NOTE: we use 'rmd' below to avoid colliding with the REMAINDER generic
;; function, and the REM standard function. Never use structs actually. Even
;; less so with null conc-names!

(defstruct (lig/kern :conc-name)
  "The Lig/Kern structure.
This structure is used to store decoded information from the lig/kern table
(see TeX: the Program [545])."
  skip next op rmd)

(defun read-lig/kern ()
  "Read one lig/kern from *stream* into a new LIG/KERN instance."
  (make-lig/kern
   :skip (read-u8)
   :next (read-u8)
   :op (read-u8)
   :rmd (read-u8)))

(defun read-o0-lig/kern ()
  "Read one lig/kern from *stream* into a new LIG/KERN instance."
  (make-lig/kern
   :skip (read-u16 nil)
   :next (read-u16 nil)
   :op (read-u16 nil)
   :rmd (read-u16 nil)))

;;; intermediate.lisp ends here
