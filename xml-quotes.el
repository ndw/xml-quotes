;;; xml-quotes.el -- read quotations from an XML document

;; Copyright (C) 2002 Norman Walsh

;; Author: Norman Walsh <ndw@nwalsh.com>
;; Maintainer: Norman Walsh <ndw@nwalsh.com>
;; Created: 2001-06-06
;; Last-Update: $Date: 2002/10/07 13:04:49 $
;; Version: 1.1
;; CVS ID: $Id: xml-quotes.el,v 1.1 2002/10/07 13:04:49 ndw Exp $
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
;;
;; I keep a collection of quotations, mostly for use in mail
;; signatures. Naturally (for me :-), I keep them in XML. These
;; functions extract quotations from this file.

;;; Prerequisites:
;;
;; This file requires xml.el by Emmanuel Briot. Recent versions of
;; emacs (at least 21.1 and beyond) include xml.el.
;;
;; You must have a quotations file. I keep mine in ~/.quotes.xml.
;;
;; <?xml version="1.0" encoding="utf-8"?>
;; <!DOCTYPE quotations
;;   PUBLIC "-//Norman Walsh//DTD Quotations V1.0//EN"
;;   "http://nwalsh.com/emacs/xmlquotes/quotations.dtd">
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

;;; Usage:
;;
;; (quotation)
;;
;; Returns the next quotation.
;;
;; (quotation n)
;;
;; Returns the n'th quotation. This sets the next quotation to n+1.

;;; Example:
;;
;; (quotation) =>
;; '(257 "Many who find the day too long, think life too short.--Charles Caleb Colton")

;;; Customization:
;;
;; This version of xml-quotes is not setup to use customization.

;;; Changes
;;
;; v1.1
;;   A bit of cleanup
;; v1.0
;;   First release

;;; Code:

(require 'xml)

(defvar xml-quote-number 0
  "Quotations are stored in a list. The xml-quote-number identifies the next quotation that will be returned.")

(defvar xml-quote-file "~/.quotes.xml"
  "The name of the file that stores the quotations")

(defvar xml-quotes nil
  "The quotations data structure.")

(defun load-quotes-file ()
  "Load the quotes file into the xml-quotes data structure."
  (let* ((xml-quotations (xml-parse-file xml-quote-file nil))
	 (children (xml-node-children (car xml-quotations))))
    (while children
      (if (and (not (stringp (car children)))
	       (string= (xml-node-name (car children)) "blockquote"))
	  (setq xml-quotes (append xml-quotes (list (car children)))))
      (setq children (cdr children)))
    xml-quotes))

(defun quote-count ()
  "Return the number of quotations in the xml-quotes data structure"
  (if (not xml-quotes)
      (load-quotes-file))
  (length xml-quotes))

(defun quote-text (quote)
  "Return the text of the specified quote."
  (let* ((para (car (xml-get-children quote 'para)))
	 (by   (quote-attribution (car (xml-get-children quote 'attribution))))
	 (text (xml-node-text para)))

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

(defun quotation (&optional num-or-random)
  "Return a quotation, with attribution.  If num-or-random is a number, return that quotation.  If it is t, return a random quotation.  Otherwise return the current quotation."
  (let ((quote nil)
	(qnum xml-quote-number)
	(attrib nil))
    (if (not xml-quotes)
	(load-quotes-file))
    (if num-or-random
	(if (numberp num-or-random)
	    (if (or (< num-or-random 0)
		    (>= num-or-random (quote-count)))
		(setq qnum 0)
	      (setq qnum num-or-random))
	  (progn
	    (random t)
	    (setq qnum (random (quote-count))))))
    (setq xml-quote-number (+ qnum 1))
    (setq quote (nth qnum xml-quotes))
    (setq attrib (quote-attribution quote))
    (if (string= attrib "")
	(list qnum (quote-text quote))
      (list qnum (concat (quote-text quote) "--" attrib)))))

(defun quote-attribution (quote)
  "Return the attribution of the specified quote."
  (let* ((attribution (car (xml-get-children quote 'attribution)))
	 (name (if attribution (car (xml-get-children attribution 'personname))))
	 (firstname (if name (car (xml-get-children name 'firstname))))
	 (surname (if name (car (xml-get-children name 'surname)))))
    (concat
     ""
     (if firstname (xml-node-text firstname))
     (if (and firstname surname) " ")
     (if surname (xml-node-text surname)))))

(defun xml-node-text (node)
  "Return the text of the specified node."
  (let ((children (xml-node-children node))
	(text ""))
    (while children
      (if (stringp (car children))
	  (setq text (concat text (car children))))
      (setq children (cdr children)))
    text))

(provide 'xml-quotes)
