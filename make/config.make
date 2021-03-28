### config.make --- Configuration part

## Copyright (C) 2019, 2021 Didier Verna

## Author: Didier Verna <didier@didierverna.net>

## This file is part of TFM.

## Permission to use, copy, modify, and distribute this software for any
## purpose with or without fee is hereby granted, provided that the above
## copyright notice and this permission notice appear in all copies.

## THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
## WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
## MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
## ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
## WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
## ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
## OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


### Commentary:



### Code:

## Installation prefix. This is used for installing TFM as follows:
# - $(PREFIX)/share/doc/tfm/ for the PDF documentation
# - $(PREFIX)/share/info/ for the info documentation
# If any of these are unsatisfactory, you will need to edit the Makefiles, or
# do the installation by hand.
PREFIX := /usr/local

## Preferred Common Lisp implementation.
# Choices are SBCL, CMUCL, CCL, ECL, CLISP, ABCL, ACL (Allegro) and LW
# (LispWorks).
LISP := SBCL

## Global Common Lisp binary cache location.
BINLOC_CACHE := ${HOME}/.cache/common-lisp

SBCL_PATH  := sbcl
CMUCL_PATH := lisp
CCL_PATH   := ccl
ECL_PATH   := ecl
# -q is needed to remove 'bye' for version.cl to work properly.
CLISP_PATH := clisp -q
ACL_PATH   := alisp
LW_PATH    := lispworks

# For ABCL, we need something slightly different.
ABCL_JAR   := /usr/local/src/common-lisp/abcl/dist/abcl.jar
JAVA       := java
JAVAC      := javac
JAR        := jar
ABCL_PATH  := abcl
SED        := sed

## Programs for generating the documentation:
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi
DVIPS    = dvips
CONVERT  = convert

### config.make ends here
