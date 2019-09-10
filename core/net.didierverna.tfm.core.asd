;;; net.didierverna.tfm.core.asd --- ASDF system definition, core library

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

(asdf:load-system :net.didierverna.tfm.setup)

(asdf:defsystem :net.didierverna.tfm.core
  :long-name "TeX Font Metrics Core"
  :description "TFM's core library"
  :long-description "The TeX Font Metrics core library.
For a more complete description of TFM, see the net.didierverna.tfm system."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/misc.php#tfm"
  :source-control "https://github.com/didierverna/tfm"
  :license "BSD"
  :version #.(net.didierverna.tfm.setup:version :short)
  :depends-on (:net.didierverna.tfm.setup)
  :serial t
  :components ((:file "meta")
	       (:module "src"
		:serial t
		:components ((:file "tfm")))))

;;; net.didierverna.tfm.core.asd ends here
