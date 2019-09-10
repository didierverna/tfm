# TFM
TFM (TeX Font Metrics) is a Common Lisp interface to the TeX Font Metrics
format. It decodes the contents of `tfm` files, and returns an abstract data
structure storing the corresponding font information in an easily accessible
way.


## Quick Start
In your favorite Lisp REPL, type this:
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
