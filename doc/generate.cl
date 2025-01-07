;;; generate.cl --- TFM reference manual generation script

;; Copyright (C) 2019, 2021, 2025 Didier Verna

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

(require "asdf")

(defconstant +introduction+
  "@macro tfm
@sc{Tfm}
@end macro

@macro cl
Common-Lisp
@end macro

@macro etc
@i{etc.}
@end macro

@tfm{} is a @cl{} interface to the @TeX{} Font Metrics format. It decodes the
contents of @code{tfm} files, and returns an abstract data structure storing
the corresponding font information in an easily accessible way.

This is the @tfm{} Reference Manual.
@ifinfo
See @xref{Top, , The @tfm{} User Manual, tfm-user},
@end ifinfo
@ifhtml
See @xref{Top, , The @tfm{} User Manual, user},
@end ifhtml
@ifnotinfo
@ifnothtml
@xref{Top, , , user, The @tfm{} User Manual},
@end ifnothtml
@end ifnotinfo
for a more human-readable guide to using @tfm{}."
  "The reference manual's introductory text.")

(asdf:load-system :net.didierverna.declt)
(net.didierverna.declt:nickname-package)

;; ASDF doesn't understand my version numbering scheme. That will change soon,
;; but in the meantime, I have to provide my version number explicitly here
;; (and so I need to load at least the setup system in order to get the
;; VERSION function).
(asdf:load-system :net.didierverna.tfm.setup)

(defvar *locations* nil)
(when (and (second sb-ext:*posix-argv*)
	   (string= (second sb-ext:*posix-argv*) "--locations"))
  (setq *locations* t))

(declt:declt :net.didierverna.tfm
	     :library-name "TFM"
	     :library-version (net.didierverna.tfm.setup:version :long)
	     :copyright-years net.didierverna.tfm.setup:*copyright-years*
	     :license :bsd
	     :introduction +introduction+
	     :locations *locations*
	     :file-name "reference"
	     :info-name "tfm-reference")

(uiop:quit)

;;; generate.cl ends here
