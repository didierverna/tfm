# Version 2.0
This version features a number of improvements at the expense of backward
incompatibility.
- LOAD-FONT now allows to override the default font name.
- Invalid custom font name and design size are now signalled by two new usage
  errors: INVALID-CUSTOM-NAME (restartable with USE-FILE-BASE-NAME) and
  INVALID-CUSTOM-DESIGN-SIZE (restartable with USE-ORIGINAL-DESIGN-SIZE).
- The LIGATURE and KERNING functions have been renamed to GET-LIGATURE and
  GET-KERN respectively.
- (UN)FREEZE now return two values: the font instance and T if (un)freezing
  did occur (NIL otherwise).
- The conditions ontology has been improved via more detailed reports and more
  explicit slot names.

## Version 1.4
- Improve representation of empty fonts. In particular, set MIN-CODE and
  MAX-CODE to NIL.
- Improve reporting of spurious char info conditions.
- Improve reporting of file-under/overflow conditions.

## Version 1.3
- Provide pointers to reference documentation in conditions report messages.
- Add a condition about non-null string tails.
- Improve handling of padded strings.

## Version 1.2
- More informative conditions report messages (including context).
- Update user manual.

### Version 1.1.1
- Fix user manual.

## Version 1.1
- Support font freezing / unfreezing.
- Keep track of the original design size.
- Reflect scaling information in font names.
- Fix one condition's report message.
- Fix some missing exports.

### Version 1.0.4
- Users can now override the original design size.
- Fix one condition's report message.

### Version 1.0.3
- Docstring fixes.

### Version 1.0.2
- Docstring fixes.

### Version 1.0.1
- Export the character-metrics symbol.

# Version 1.0
- First public release.
