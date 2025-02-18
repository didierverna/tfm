# TFM
TFM (for TeX Font Metric) is the standard font description format used by TeX.
The TFM library parses and decodes TeX Font Metric and similar formats into an
abstract data structure, providing easy access to the corresponding font
information in Common Lisp.

In addition to regular TFM data, the library also supports level 0 Omega (OFM)
fonts.

## Quick Start
In your favorite Lisp REPL, type something like this:
```
(asdf:load-system :net.didierverna.tfm)
(net.didierverna.tfm:nickname-package)
(defvar *cmr10* (tfm:load-font #p"/path/to/cmr10.tfm"))
```
You will end up with a `font` object, containing the decoded font information,
and stored in an easily accessible way. Inspect the object in question to
familiarize yourself with its contents.

## More information
TFM comes with both a
[user manual](https://www.lrde.epita.fr/~didier/software/lisp/tfm/user/)
and a
[reference manual](https://www.lrde.epita.fr/~didier/software/lisp/tfm/reference/).
Please see the projet's
[homepage](https://www.lrde.epita.fr/~didier/software/lisp/typesetting.php#tfm)
for more information.
