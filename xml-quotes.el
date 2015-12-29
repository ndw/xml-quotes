;;; xml-quotes.el --- read quotations from an XML document

;; Copyright (C) 2002,2015 Norman Walsh

;; Author: Norman Walsh <ndw@nwalsh.com>
;; URL: https://github.com/ndw/xml-quotes
;; Created: 2001-06-06
;; Last-Update: 2015-12-29
;; Version: 1.2
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
;; (xmlq-quotation)
;;
;; Returns the next quotation.
;;
;; (xmlq-quotation n)
;;
;; Returns the n'th quotation. This sets the next quotation to n+1.

;;; Example:
;;
;; (xmlq-quotation) =>
;; '(257 "Many who find the day too long, think life too short.--Charles Caleb Colton")

;;; Customization:
;;
;; This version of xml-quotes is not setup to use customization.

;;; Changes
;;
;; v1.2
;;   Turns out 1.1 was pretty seriously broken. I fixed that. And I renamed all
;;   of the functions and variables to be in the xmlq- namespace.
;; v1.1
;;   A bit of cleanup
;; v1.0
;;   First release

;;; Code:

(require 'xml)

(defvar xmlq-quote-number 0
  "Quotations are stored in a list. The xmlq-quote-number identifies the next quotation that will be returned.")

(defvar xmlq-quote-file "~/.quotes.xml"
  "The name of the file that stores the quotations")

(defvar xmlq--quotes nil
  "The quotations data structure.")

(defun xmlq--load-quotes-file ()
  "Load the quotes file into the xmlq--quotes data structure."
  (let* ((xmlq-quotations (xml-parse-file xmlq-quote-file nil))
	 (children (xml-node-children (car xmlq-quotations))))
    (while children
      (if (and (not (stringp (car children)))
	       (string= (xml-node-name (car children)) "quote"))
	  (setq xmlq--quotes (append xmlq--quotes (list (car children)))))
      (setq children (cdr children)))
    xmlq--quotes))

(defun xmlq-quote-count ()
  "Return the total number of quotations."
  (if (not xmlq--quotes)
      (xmlq--load-quotes-file))
  (length xmlq--quotes))

(defun xmlq-quote-text (quote)
  "Return the text of the specified quote."
  (let* ((text (car (cddr quote)))
	 (by   (xmlq-quote-attribution (car (xml-get-children quote 'attribution)))))
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

(defun xmlq-quotation (&optional num-or-random)
  "Return a quotation, with attribution.  If num-or-random is a number, return that quotation.  If it is t, return a random quotation.  Otherwise return the current quotation."
  (let ((quote nil)
	(qnum xmlq-quote-number)
	(attrib nil))
    (if (not xmlq--quotes)
	(xmlq--load-quotes-file))
    (if num-or-random
	(if (numberp num-or-random)
	    (if (or (< num-or-random 0)
		    (>= num-or-random (xmlq-quote-count)))
		(setq qnum 0)
	      (setq qnum num-or-random))
	  (progn
	    (random t)
	    (setq qnum (random (xmlq-quote-count)))))
      (setq xmlq-quote-number
            (if (>= qnum (1- (xmlq-quote-count)))
                0
              (+ qnum 1))))
    (setq quote (nth qnum xmlq--quotes))
    (setq attrib (xmlq-quote-attribution quote))
    (if (string= attrib "")
	(list qnum (xmlq-quote-text quote))
      (list qnum (concat (xmlq-quote-text quote) "--" attrib)))))

(defun xmlq-quote-attribution (quote)
  "Return the attribution of the specified quote."
  (xml-get-attribute quote 'by))

(provide 'xml-quotes)
;;; xml-quotes ends here
