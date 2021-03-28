### include.make --- Prologue Makefile

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

## Contents management by FCM version 0.1.


### Code:

PROJECT   := tfm
PACKAGE   := net.didierverna.$(PROJECT)
ASDF_FILE := $(PACKAGE).asd

TYPESET_COPYRIGHT_YEARS := $(subst -,--,$(COPYRIGHT_YEARS))

PERL := perl

SHARE := $(PREFIX)/share

W3DIR := $(HOME)/www/software/lisp/$(PROJECT)


SBCL_CACHE  := sbcl
SBCL_BINLOC := sbcl
SBCL_LOAD   := --load
SBCL_EVAL   := --eval

CMUCL_CACHE  := cmu
CMUCL_BINLOC := cmu
CMUCL_LOAD   := -load
CMUCL_EVAL   := -eval

CCL_CACHE  := ccl
CCL_BINLOC := openmcl
CCL_LOAD   := --load
CCL_EVAL   := --eval

ECL_CACHE  := ecl
ECL_BINLOC := ecl
ECL_LOAD   := -load
ECL_EVAL   := -eval

CLISP_CACHE  := clisp
CLISP_BINLOC := clisp
CLISP_LOAD   := -i
CLISP_EVAL   := -x

ABCL_CACHE  := abcl
ABCL_BINLOC := abcl
ABCL_LOAD   := --load
ABCL_EVAL   := --eval

ACL_CACHE  := acl
ACL_BINLOC := acl
ACL_LOAD   := -L
ACL_EVAL   := -e

LW_CACHE  := lw
LW_BINLOC := lw
LW_LOAD   := -load
LW_EVAL   := -eval

BINLOC := $($(LISP)_BINLOC)

### prologue.make ends here
