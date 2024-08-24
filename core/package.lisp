;;; package.lisp --- TFM package definition

;; Copyright (C) 2019, 2021, 2024 Didier Verna

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
  (:export

    ;; From the :net.didierverna.tfm.setup package:
    :*copyright-years*
    :*release-major-level*
    :*release-minor-level*
    :*release-status*
    :*release-status-level*
    :*release-name*
    :version

    ;; From package.lisp (this file):
    :nickname-package

    ;; From src/util.lisp:
    :tfm :tfm-warning :tfm-error :tfm-compliance :tfm-usage
    :tfm-compliance-warning :tfm-compliance-error
    :tfm-usage-warning :tfm-usage-error
    :u16-overflow :value
    :fix-word-overflow :value :set-to-zero
    :invalid-padded-string-length :value :pad
    :invalid-padded-string :value
    :padded-string-overflow :value
    :read-maximum-length :keep-string :fix-string :discard-string

    ;; From src/character.lisp:
    :character-metrics
    :code :font :width :height :depth :italic-correction :next-character
    :extensiblep :not-extensible :value
    :top-character :middle-character :bottom-character :repeated-character

    ;; From src/font.lisp:
    :ligature :composite :delete-before :delete-after :pass-over
    :font :name :file :checksum :frozen :design-size :original-design-size
    :encoding :family :7bits-safe :face-number :weight :slope :expansion
    :face-code
    :slant :interword-space :interword-stretch :interword-shrink :ex :em
    :extra-space :parameters :min-code :max-code :character-count
    :boundary-character
    :invalid-character-code :value
    :get-character
    :different-fonts :character1 :character2
    :ligature :kerning
    :freeze :unfreeze
    :math-symbols-font
    :num1 :num2 :num3 :denom1 :denom2 :sup1 :sup2 :sup3 :sub1 :sub2
    :supdrop :subdrop :delim1 :delim2 :axis-height
    :math-extension-font :default-rule-thickness
    :big-op-spacing1 :big-op-spacing2 :big-op-spacing3 :big-op-spacing4
    :big-op-spacing5
    :l0-omega-font :direction

    ;; From src/file.lisp:
    :invalid-design-size :value :set-to-ten
    :name
    :invalid-table-index :value :largest
    :invalid-ligature-opcode :value
    :abort-lig/kern-program :discard-ligature :discard-kerning
    :abort-lig/kern-program
    :spurious-char-info :value
    :invalid-table-start :value
    :no-boundary-character
    :character-list-cycle :value
    :ligature-cycle :value :characters
    :set-to-zero :abort-lig/kern-program
    :discard-next-character :discard-extension-recipe
    :discard-next-character :discard-ligature
    :declared-size :actual-size
    :file-underflow :file-overflow
    :invalid-header-length :value
    :invalid-character-range :bc :ec
    :invalid-section-lengths :lf :lh :nc :nw :nh :nd :ni :nl :nk :ne :np
    :invalid-l0-ofm-section-lengths
    :invalid-table-length :value :smallest :largest
    :invalid-ofm-level :value
    :extended-tfm :value :file
    :load-font :cancel-loading))


(in-package :net.didierverna.tfm)

(defun nickname-package (&optional (nickname :tfm))
  "Add NICKNAME (:TFM by default) to the :NET.DIDIERVERNA.TFM package."
  (rename-package :net.didierverna.tfm
		  (package-name :net.didierverna.tfm)
		  (adjoin nickname (package-nicknames :net.didierverna.tfm)
			  :test #'string-equal)))

;;; package.lisp ends here
