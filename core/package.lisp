;;; package.lisp --- TFM package definition

;; Copyright (C) 2019, 2021, 2024, 2025 Didier Verna

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
  (:documentation "The TeX Font Metric package.")
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
    :context :context-string
    :tfm :context :tfm-warning :tfm-error
    :tfm-compliance :reference
    :tfm-usage
    :tfm-compliance-warning :tfm-compliance-error
    :tfm-usage-warning :tfm-usage-error
    :u16-overflow :value
    :u32-overflow :value
    :fix-word-overflow :value
    :set-to-zero
    :padded-string
    :invalid-padded-string-length :value :pad
    :invalid-padded-string :str
    :padded-string-overflow :overflow
    :read-maximum-length :discard-string :keep-string :fix-string

    ;; From src/intermediate.lisp:
    :spurious-char-info :char-info :tag :remainder

    ;; From src/character.lisp:
    :character-metrics
    :code :font :width :height :depth :italic-correction :next-character
    :extensiblep :not-extensible :chr
    :top-character :middle-character :bottom-character :repeated-character

    ;; From src/font.lisp:
    :ligature :composite :delete-before :delete-after :pass-over
    :font :file :name :checksum :frozen :design-size :original-design-size
    :encoding :family :7bits-safe :face-number :weight :slope :expansion
    :face-code
    :slant :interword-space :interword-stretch :interword-shrink :ex :em
    :extra-space :parameters :min-code :max-code :character-count
    :boundary-character
    :invalid-custom-name :name
    :invalid-custom-design-size :value
    :use-file-base-name :use-original-design-size
    :invalid-character-code :code
    :get-character
    :different-fonts :character1 :character2
    :get-ligature :get-kern
    :freeze :unfreeze
    :math-symbols-font
    :num1 :num2 :num3 :denom1 :denom2 :sup1 :sup2 :sup3 :sub1 :sub2
    :supdrop :subdrop :delim1 :delim2 :axis-height
    :math-extension-font :default-rule-thickness
    :big-op-spacing1 :big-op-spacing2 :big-op-spacing3 :big-op-spacing4
    :big-op-spacing5
    :o0-font :direction

    ;; From src/file.lisp:
    :invalid-design-size :value
    :invalid-original-design-size :value
    :padded-string-context :name
    :set-to-ten
    :tfm-table-error :name
    :invalid-table-index :index :largest
    :invalid-ligature-opcode :opcode
    :abort-lig/kern-program :discard-ligature :discard-kern
    :invalid-table-start :value
    :no-boundary-character
    :character-list-cycle :character-list
    :ligature-cycle :ligature :characters
    :table-context :name :index :size
    :char-info-table-context :code
    :set-to-zero :abort-lig/kern-program
    :discard-next-character :discard-extension-recipe :discard-ligature
    :file-size-mixin :declared-size :actual-size
    :file-underflow :file-overflow
    :invalid-header-length :value
    :invalid-character-range :bc :ec
    :invalid-section-lengths :lf :lh :nc :nw :nh :nd :ni :nl :nk :ne :np
    :invalid-o0-section-lengths
    :invalid-table-length :value :smallest :largest
    :invalid-ofm-level :value
    :unsupported-format :fmt :file
    :load-font :cancel-loading))


(in-package :net.didierverna.tfm)

(defun nickname-package (&optional (nickname :tfm))
  "Add NICKNAME (:TFM by default) to the :NET.DIDIERVERNA.TFM package."
  (rename-package :net.didierverna.tfm
		  (package-name :net.didierverna.tfm)
		  (adjoin nickname (package-nicknames :net.didierverna.tfm)
			  :test #'string-equal)))

;;; package.lisp ends here
