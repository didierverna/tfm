;;; meta.lisp --- Meta utilities

;; Copyright (C) 2019 Didier Verna

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
  (:use :cl :net.didierverna.tfm.setup)
  (:shadow :*readtable*)
  (:export
    ;; From the :net.didierverna.tfm.setup package:
    :*release-major-level*
    :*release-minor-level*
    :*release-status*
    :*release-status-level*
    :*release-name*
    :version
    ;; From meta.lisp (this file):
    :nickname-package
    ;; From src/util.lisp
    :tfm :tfm-warning :tfm-error
    :tfm-compliance-warning :tfm-compliance-error
    :tfm-usage-warning :tfm-usage-error
    :u16-overflow :fix-word-overflow :value :set-to-zero
    :invalid-string-length :invalid-bcpl-string :padding
    :read-maximum-length :discard-string :keep-string :fix-string
    ;; From src/character-metrics.lisp:
    :code :width :height :depth :italic-correction
    :next-character
    :extensiblep :not-extensible
    :top-character :middle-character :bottom-character :repeated-character
    ;; Font src/font.lisp:
    :composite :delete-before :delete-after :pass-over
    :font :name :file :checksum
    :encoding :family
    :7bits-safe :face-number :face-code :weight :slope :expansion
    :design-size :slant
    :interword-space :interword-stretch :interword-shrink :ex :em :extra-space
    :min-code :max-code :character-count
    :boundary-character
    :parameters
    :get-character :ligature :kerning
    :math-symbols-font
    :num1 :num2 :num3
    :denom1 :denom2
    :sup1 :sup2 :sup3
    :sub1 :sub2
    :supdrop :subdrop
    :delim1 :delim2
    :axis-height
    :math-extension-font
    :default-rule-thickness
    :big-op-spacing1 :big-op-spacing2 :big-op-spacing3 :big-op-spacing4
    :big-op-spacing5
    ;; From src/file.lisp:
    :invalid-design-size :set-to-ten
    :invalid-char-info
    :invalid-table-start :name
    :no-boundary-character :discard-lig/kern
    :character-list-cycle
    :file-underflow :file-overflow :declared-size :actual-size
    :invalid-header-length
    :invalid-character-range :bc :ec
    :invalid-section-lengths :lf :lh :nc :nw :nh :nd :ni :nl :nk :ne :np
    :invalid-table-length :smallest :largest
    :extended-tfm
    :load-font))

(in-package :net.didierverna.tfm)


;; -------------------
;; External utilities:
;; -------------------

(defun nickname-package (&optional (nickname :tfm))
  "Add NICKNAME (:TFM by default) to the :NET.DIDIERVERNA.TFM package."
  (rename-package :net.didierverna.tfm
		  (package-name :net.didierverna.tfm)
		  (adjoin nickname (package-nicknames :net.didierverna.tfm)
			  :test #'string-equal)))


;; -------------------
;; Internal utilities:
;; -------------------

(defvar *readtable* (copy-readtable)
  "The TFM readtable.")


;; String concatenation
;; --------------------
(defun tilde-reader (stream char)
  "Read a series of ~\"string\" to be concatenated together."
  (declare (ignore char))
  (flet ((read-string (&aux (string (read stream t nil t)))
	   (check-type string string "a string")
	   string))
    (apply #'concatenate 'string
	   (read-string)
	   (loop :while (char= (peek-char t stream nil nil t) #\~)
		 :do (read-char stream t nil t)
		 :collect (read-string)))))

(set-macro-character #\~ #'tilde-reader nil *readtable*)

;; Emacs indentation
;; -----------------
(defun clindent (symbol indent)
  "Set SYMBOL's indentation to INDENT in Emacs.
This function sets SYMBOL's common-lisp-indent-function property.
If INDENT is a symbol, use its indentation definition.
Otherwise, INDENT is considered as an indentation definition."
  (when (and (member :swank *features*)
	     (configuration :swank-eval-in-emacs))
    (funcall (intern "EVAL-IN-EMACS" :swank)
	     `(put ',symbol 'common-lisp-indent-function
		   ,(if (symbolp indent)
			`(get ',indent 'common-lisp-indent-function)
		      `',indent))
	     t)))

(defmacro defindent (symbol indent)
  "Set SYMBOL's indentation to INDENT in (X)Emacs.
SYMBOL and INDENT need not be quoted.
See CLINDENT for more information."
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (clindent ',symbol ',indent)))

(defun i-reader (stream subchar arg)
  "Read an argument list for the DEFINDENT macro."
  (declare (ignore subchar arg))
  (cons 'defindent (read stream)))

(set-dispatch-macro-character #\# #\i #'i-reader *readtable*)


(defmacro in-readtable (name)
  "Set the current readtable to the value of NAME::*READTABLE*."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf cl:*readtable* (symbol-value (find-symbol "*READTABLE*" ,name)))))

;;; meta.lisp ends here
