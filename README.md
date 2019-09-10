# TFM
TFM (for TeX Font Metrics) is the standard font description format used by
TeX. The TFM library parses and decodes TFM files into an abstract data
structure, providing easy access to the corresponding font information in
Common Lisp.

## Quick Start
In your favorite Lisp REPL, type something like this:
```
(asdf:load-system :net.didierverna.tfm)
(net.didierverna.tfm:nickname-package)
(defvar *cmr10* (tfm:parse #p"/path/to/cmr10.tfm"))
```
You will end up with a `tfm` object, containing the decoded font information,
and stored in an easily accessible way. Inspect the object in question to
familiarize yourself with its contents.

## More information
TFM comes with both a [user manual]
(https://www.lrde.epita.fr/~didier/software/lisp/tfm/user/)
and a [reference manual]
(https://www.lrde.epita.fr/~didier/software/lisp/tfm/reference/).
Please see the projet's [homepage]
(https://www.lrde.epita.fr/~didier/software/lisp/misc.php#tfm)
for more information.
