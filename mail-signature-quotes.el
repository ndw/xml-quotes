;;; mail-signature-quotes.el -- format quotations for mail signatures

;; Copyright (C) 2002 Norman Walsh

;; Author: Norman Walsh <ndw@nwalsh.com>
;; Maintainer: Norman Walsh <ndw@nwalsh.com>
;; Created: 2001-06-06
:; Last-Update: $Date: 2002/10/14 12:43:30 $
;; Version: 1.1
;; CVS ID: $Id: mail-signature-quotes.el,v 1.2 2002/10/14 12:43:30 ndw Exp $
;; Keywords: mail signature quotations

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
;; This code formats mail signature blocks with quotations.

;;; Prerequisites:
;;
;; This file expects to get quotations from xml-quotes.el, but you could
;; define your own quotation source.
;;
;; Some of these functions assume that you'll be using Gnus and BBDB.
;; If you aren't, the default fallback is pretty lame. You'll probably want
;; to do something abou that.
;;

;;; Usage:
;;
;; (mail-signature "filename")
;;
;; Formats the signature block in "filename" with a quotation and
;; returns it.
;;
;; (mail-signature "filename" 4)
;;
;; Formats the signature block in "filename" with quotation number 4
;; and returns it.
;;
;; (add-mail-signature)
;;
;; Formats the signature block returned by (default-signature) with a
;; quotation and adds it to the bottom of the current buffer. If an
;; existing signature block is already present, it'll be stripped away
;; first.
;;
;; (add-mail-signature 4)
;;
;; Formats the signature block returned by (default-signature) with
;; quotation number 4 and adds it to the bottom of the current buffer.
;; If an existing signature block is already present, it'll be
;; stripped away first.

;;; Customization:
;;
;; This version of mail-signature-quotes is not setup to use customization.

;;; Changes
;;
;; v1.1
;;   A bit of cleanup
;; v1.0
;;   First release

;;; Code:

(require 'xml-quotes)

(defvar message-signature-file "~/.signature"
  "The default signature file")

(defvar closing-name "John Doe"
  "The name to be used in the closing")

(defvar closing-text-alist '(("my-spouse@example.com" "Love,")
			     ("a-friend@example.com"
			      ("Chau," "Ciao," "Cheers," "Later,"
			       "Au revoir," "A bientot,"))
			     ("#default" "Be seeing you,"))
  "An a list of closings")

(defvar group-no-closing '()
  "These Gnus groups get no closing")

;; ================================================================================

(defvar default-signature-function 'generic-default-signature
  "What function should be called to generate a signature")

(defun gnus-default-signature ()
  "This function uses Gnus gnus-posting-styles to get the mail signature"
  (interactive)
  (let* ((default-sig "default")
	 (group-name (if gnus-newsgroup-name
			 gnus-newsgroup-name
		       "default"))
	 (signature nil))
    (dolist (style gnus-posting-styles)
      (setq match (pop style))
      (if (and match (stringp match))
	  (if (string-match match group-name)
	      (dolist (attribute style)
		(setq element (pop attribute))
		(if (eq element 'signature)
		    (setq signature (car attribute)))))))
    (if (and signature (eq (car signature) 'mail-signature))
	(cadr signature)
      (format "~/.signatures/%s" default-sig))))

(defun generic-default-signature ()
  "This function just returns message-signature-file"
  message-signature-file)

(defun default-signature ()
  (funcall default-signature-function))

;; ================================================================================

(defvar signature-override-function 'nil-signature-override
  "What function should be called to override a quotation")

(defun nil-signature-override (email quote)
  nil)

;; ================================================================================

(defun add-mail-signature (&optional quotenum)
  "Inserts my signature and a mail quote"
  (interactive "P")
  (let* ((sigfile (default-signature)))
    (insert-mail-signature sigfile quotenum)))

(defun insert-mail-signature (sigfile &optional quotenum)
  "Inserts sigfile with a random mail quote"
  (let* ((sigregexp (concat "\n-- ")))
    (save-excursion
      ;; If we already have a sig in here, get rid of it...
      (goto-char (point-min))
      (if (re-search-forward sigregexp nil t nil)
	  (progn
	    (beginning-of-line)
	    (next-line 1)
	    (push-mark nil t t)
	    (goto-char (point-max))
	    (delete-region (mark) (point))))
      (goto-char (point-max))
      (insert (mail-signature sigfile quotenum)))))

;; ================================================================================

(defun gnus-insert-closing-hook ()
  "Closing hook for Gnus to insert the message closing"
  (let* ((group-name (if gnus-newsgroup-name
			 gnus-newsgroup-name
		       ""))
	 ;; don't downcase nil
	 (primary    (if (mail-to-primary)
			 (downcase (mail-to-primary))
		       ""))
	 (closing-text-data (if (assoc primary closing-text-alist)
				(car (cdr
				      (assoc primary closing-text-alist)))
			      (car (cdr
				    (assoc "#default"
					   closing-text-alist)))))
	 (closing-text (if (listp closing-text-data)
			   (nth (random (length closing-text-data))
				closing-text-data)
			 closing-text-data)))
    (if (not (member group-name group-no-closing))
	(progn
	  (goto-char (point-max))
	  (newline)
	  (insert (concat (make-string 40 ? ) closing-text "\n"))
	  (insert (concat (make-string 42 ? ) closing-name))))))

;; ================================================================================

(defun mail-signature (sigfile &optional quotenum)
  "Returns a formatted mail signature"
  (formatted-signature sigfile (signature-quote quotenum)))

(defun formatted-signature (sig-file quote &optional line-length pad-lines)
  "Signatures come in two forms: block style which have a vertical line of | characters and line style which do not. Based on the specified signature, format the quotation correctly."
  (let ((scratch-buf (generate-new-buffer "*signature*"))
	(this-buf (current-buffer))
	(lines ())
	(block-sig t))
    (set-buffer scratch-buf)
    (insert-file-contents sig-file nil)
    (end-of-line)
    (setq block-sig (search-backward "|" nil t nil))
    (end-of-line)
    (setq lines (if block-sig
		    (block-formatted-signature sig-file quote line-length pad-lines)
		  (line-formatted-signature sig-file quote line-length pad-lines)))
    (set-buffer-modified-p nil)
    (set-buffer this-buf)
    (kill-buffer scratch-buf)
    lines))

(defun block-formatted-signature (sig-file quote &optional line-length pad-lines)
  "Format a quotation as a block, to the right of the |'s"
  (let ((lines ())
	(width 40)
	(sig-width 40)
	(fill-width line-length))
    (if (not fill-width)
	(setq fill-width 72))

    (setq sig-width (current-column))
    (setq width (- fill-width sig-width))
    (setq lines (string-wrap quote width))
    (while lines
      (if (< (current-column) sig-width)
	  (progn
	    (if pad-lines
		(progn 
		  (insert (car pad-lines))
		  (setq pad-lines (cdr pad-lines))))
	    (while (< (current-column) sig-width)
	      (insert " "))
	    (backward-delete-char-untabify 3)
	    (insert " | ")))
      (insert (car lines))
      (setq lines (cdr lines))
      (if (> (forward-line) 0)
	  (insert "\n"))
      (end-of-line))

    (setq lines (buffer-string))

    ;; Return the quotation
    lines))

(defun line-formatted-signature (sig-file quote &optional line-length pad-lines)
  "Format a quotation as a block, below the quotation, extending across the page"
  (let ((lines ())
	(width 72))
    (newline)
    (setq lines (string-wrap quote width))
    (while lines
      (insert "* ");
      (insert (car lines))
      (setq lines (cdr lines))
      (if (> (forward-line) 0)
	  (insert "\n"))
      (end-of-line))

    (setq lines (buffer-string))

    ;; Return the quotation
    lines))

;; ================================================================================

(defvar signature-quote-number t)

(defun next-signature-quote (emailaddr)
  "Return the next signature quote number for this person, if they appear in my bbdb. Otherwise return signature-quote-number and increment it"
   (let ((qnum (random (quote-count))))
     ;; don't bbdb-search if emailaddr is nil
     (if (and emailaddr (fboundp 'bbdb-message-search))
	 (let ((rec (car (bbdb-message-search nil emailaddr))))
	   (if rec
	       (progn
		 (setq qnum (if (bbdb-record-get-field rec 'last-email-quote)
				(string-to-number
				 (bbdb-record-get-field rec 'last-email-quote))
			      1))
		 (bbdb-record-set-field rec 'last-email-quote (number-to-string
							     (increment-quote-number qnum))))
	     (if (numberp signature-quote-number)
		 (setq qnum (increment-quote-number signature-quote-number))))))
     (setq signature-quote-number qnum)))

(defun increment-quote-number (qnum)
  "Increment quote number, but wrap around when we reach the end"
  (if (>= qnum (quote-count))
      0
    (+ qnum 1)))

(defun set-signature-quote (&optional prefixarg)
  (interactive "P")
  (if prefixarg
      (if (numberp prefixarg)
	  (setq signature-quote-number prefixarg)
	(setq signature-quote-number t)))
  (if (numberp signature-quote-number)
      (message "Next signature quote is #%d" signature-quote-number)
    (message "Next signature quote is random")))

(defun mail-to-primary ()
  "Return the email address of the primary recipient. On a reply, this is the From: field of the original message, otherwise it's the first person on the To: field of this message"
  (if (and (boundp 'gnus-article-reply) gnus-article-reply)
      (save-excursion
	(set-buffer gnus-article-buffer)
	(car
	 (cdr
	  (mail-extract-address-components
	   (message-fetch-field
	    "From")))))
    (if (fboundp 'message-fetch-field)
	(save-excursion
	  (let ((addr (message-fetch-field "To")))
	    (if addr
		(car (cdr (mail-extract-address-components addr)))
	      "#default")))
      "#default")))

(defun signature-quote (&optional explicit-quote-num)
  (let* ((emailaddr (mail-to-primary))
	 (quote-num (if explicit-quote-num
			explicit-quote-num
		      (next-signature-quote emailaddr)))
	 quote-list quote)
    (setq quote-list (quotation quote-num))
    (setq quote-num (car quote-list))

    (if (boundp 'signature-override-function)
	(setq quote (funcall signature-override-function emailaddr (cadr quote-list))))

    (if quote
	(message (format "Using override quotation for %s" emailaddr))
      (progn
	(setq quote (cadr quote-list))
	(message (format "Quotation #%d for %s" quote-num emailaddr))))

    quote))

;; ======================================================================

(defun string-wrap (string &optional width)
  "Wrap string into a lines of width no larger than width."
  (let ((line-list ())
	(line "")
	(count 0))
    (while (> (length string) width)
      (setq count width)
      (setq line (substring string 0 count))
      ;;(insert (format "(%s)\n" line))
      (while (and (> (length line) 0) 
		  (not (string= (substring line -1 nil) " ")))
	(setq count (- count 1))
	(setq line (substring string 0 count)))
      (if (= (length line) 0)
	  (progn 
	    (setq count width)
	    (setq line (substring string 0 count)))
	(setq line (substring string 0 (- count 1))))
      ;;(insert (format "[%s]\n" line))
      (setq line-list (append line-list (list line)))
      (setq string (substring string count)))
    (append line-list (list string))))

;; ======================================================================

(provide 'mail-signature-quotes);
