# Emacs XML Quotations

Emacs XML Quotations is a set of macros for generating quotations at
the end of email messages. At least, that's what I use it for.
Integration with Gnus and BBDB is provided. Hopefully the “API”
exposed to extract quotations is general enough to integrate into
other tools.

* `xml-quotes.el`, functions for reading the XML Quotations file. This is the
   primary entry point. Functions in this module can read and return quotations.
* `mail-signature-quotes.el`, functions for adding quotations to the end of email messages.
* `quotations.rnc`, a RELAX NG grammar for the quotations document.
* `quotes.xml`, a sample quotations file.

There are a few `defvar`s at the top of the `.el` files that you should consult.
In particular, if you don't store the quotations in a file called
`~/.quotes.xml`, you'll have to update `xmlq-quote-file`.

Share and enjoy.

## Changes in version 1.2

* Fixed bugs caused by an incomplete transition to a new XML format in 1.1.
* Renamed all of the variables and functions to use the `xmlq-`
  namespace. If you've been using 1.1 (which is very unlikely
  considering the bugs), you'll have to double-check your customizations.
