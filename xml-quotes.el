;;; xml-quotes.el --- read quotations from an XML document

;; Copyright (C) 2002,2015 Norman Walsh

;; Author: Norman Walsh <ndw@nwalsh.com>
;; URL: https://github.com/ndw/xml-quotes
;; Created: 2001-06-06
;; Last-Update: 2015-12-30
;; Version: 1.3
;; Keywords: xml quotations

;; This file is NOT part of GNU emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; I keep a collection of quotations, mostly for use in mail
;; signatures. Naturally (for me :-), I keep them in XML. These
;; functions extract quotations from this file.
;;
;; Prerequisites:
;;
;; This file requires xml.el by Emmanuel Briot. Recent versions of
;; emacs (at least 21.1 and beyond) include xml.el.
;;
;; You must have a quotations file. I keep mine in ~/.quotes.xml.
;;
;; <?xml version="1.0" encoding="utf-8"?>
;; <quotations>
;; <quote>Everything should be made as simple as possible, but no
;; simpler.</quote>
;;
;; <quote by="Robert Frost">We dance around in a ring and suppose,
;; but the Secret sits in the middle and knows.</quote>
;;
;; <!-- ... -->
;;
;; </quotations>
;;
;; Usage:
;;
;; (xml-quotes-quotation)
;;
;; Returns the next quotation.
;;
;; (xml-quotes-quotation n)
;;
;; Returns the n'th quotation. This sets the next quotation to n+1.

;;; Example:
;;
;; (xml-quotes-quotation) =>
;; '(257 "Many who find the day too long, think life too short.--Charles Caleb Colton")

;;; Changes
;;
;; v1.3
;;   Renamed to xml-quotes- namespace per conventions; added autoloads; changed
;;   defvars into defcustoms.
;; v1.2
;;   Turns out 1.1 was pretty seriously broken. I fixed that. And I renamed all
;;   of the functions and variables to be in the xmlq- namespace.
;; v1.1
;;   A bit of cleanup
;; v1.0
;;   First release

;;; Code:

(require 'xml)

(defcustom xml-quotes-quote-file "~/.quotes.xml"
  "The name of the file that stores the quotations"
  :type 'string
  :group 'xml-quotes)

(defvar xml-quotes--quotes nil
  "The quotations data structure.")

(defvar xml-quotes--quote-number 0
  "Quotations are stored in a list. The xml-quotes--quote-number identifies the next quotation that will be returned.")

(defun xml-quotes--load-quotes-file ()
  "Load the quotes file into the xml-quotes--quotes data structure."
  (let* ((xml-quotes-quotations (xml-parse-file xml-quotes-quote-file nil))
	 (children (xml-node-children (car xml-quotes-quotations))))
    (while children
      (if (and (not (stringp (car children)))
	       (string= (xml-node-name (car children)) "quote"))
	  (setq xml-quotes--quotes (append xml-quotes--quotes (list (car children)))))
      (setq children (cdr children)))
    xml-quotes--quotes))

;;;###autoload
(defun xml-quotes-quote-count ()
  "Return the total number of quotations."
  (if (not xml-quotes--quotes)
      (xml-quotes--load-quotes-file))
  (length xml-quotes--quotes))

(defun xml-quotes-quote-text (quote)
  "Return the text of the specified quote."
  (let* ((text (car (cddr quote)))
	 (by   (xml-quotes-quote-attribution (car (xml-get-children quote 'attribution)))))
    ;; Replace all tabs and newlines in text with spaces; squash multiple spaces.
    ;; there's gotta be a better way...
    (let ((count 0))
      (while (< count (length text))
	(progn
	  (if (or
	       (= (string-to-char (substring text count (+ count 1))) ?\n)
	       (= (string-to-char (substring text count (+ count 1))) ?\t))
	      (setq text (concat
			  (substring text 0 count)
			  " "
			  (substring text (+ count 1)))))
	  (setq count (+ count 1))))
      (if (string-match "^ +" text)
	  (setq text (substring text (match-end 0))))
      (if (string-match " +$" text)
	  (setq text (substring text 0 (string-match " *$" text))))
      (while (setq count (string-match "  +" text))
	(setq text (concat
		    (substring text 0 count)
		    " "
		    (substring text (match-end 0)))))
      text)))

;;;###autoload
(defun xml-quotes-quotation (&optional num-or-random)
  "Return a quotation, with attribution.  If num-or-random is a number, return that quotation.  If it is t, return a random quotation.  Otherwise return the current quotation."
  (let ((quote nil)
	(qnum xml-quotes--quote-number)
	(attrib nil))
    (if (not xml-quotes--quotes)
	(xml-quotes--load-quotes-file))
    (if num-or-random
	(if (numberp num-or-random)
	    (if (or (< num-or-random 0)
		    (>= num-or-random (xml-quotes-quote-count)))
		(setq qnum 0)
	      (setq qnum num-or-random))
	  (progn
	    (random t)
	    (setq qnum (random (xml-quotes-quote-count)))))
      (setq xml-quotes--quote-number
            (if (>= qnum (1- (xml-quotes-quote-count)))
                0
              (+ qnum 1))))
    (setq quote (nth qnum xml-quotes--quotes))
    (setq attrib (xml-quotes-quote-attribution quote))
    (if (string= attrib "")
	(list qnum (xml-quotes-quote-text quote))
      (list qnum (concat (xml-quotes-quote-text quote) "--" attrib)))))

(defun xml-quotes-quote-attribution (quote)
  "Return the attribution of the specified quote."
  (xml-get-attribute quote 'by))

(provide 'xml-quotes)
;;; xml-quotes.el ends here
