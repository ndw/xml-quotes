;;; mail-signature-quotes.el -- format quotations for mail signatures

;; Copyright (C) 2002,2015 Norman Walsh

;; Author: Norman Walsh <ndw@nwalsh.com>
;; URL: https://github.com/ndw/xml-quotes
;; Created: 2001-06-06
;; Last-Update: 2015-12-29
;; Version: 1.2
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
;;
;; Prerequisites:
;;
;; This file expects to get quotations from xml-quotes.el, but you could
;; define your own quotation source.
;;
;; Some of these functions assume that you'll be using Gnus and BBDB.
;; If you aren't, the default fallback is pretty lame. You'll probably want
;; to do something abou that.
;;
;;
;; Usage:
;;
;; (xmlq-mail-signature "filename")
;;
;; Formats the signature block in "filename" with a quotation and
;; returns it.
;;
;; (xmlq-mail-signature "filename" 4)
;;
;; Formats the signature block in "filename" with quotation number 4
;; and returns it.
;;
;; (xmlq-add-mail-signature)
;;
;; Formats the signature block returned by (default-signature) with a
;; quotation and adds it to the bottom of the current buffer. If an
;; existing signature block is already present, it'll be stripped away
;; first.
;;
;; (xmlq-add-mail-signature 4)
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
;; v1.2
;;   Renamed all of the functions and variables to be in the xmlq- namespace.
;; v1.1
;;   A bit of cleanup
;; v1.0
;;   First release

;;; Code:

(require 'xml-quotes)

(defvar xmlq-message-signature-file "~/.signature"
  "The default signature file")

(defvar xmlq-closing-name "John Doe"
  "The name to be used in the closing")

(defvar xmlq-closing-text-alist '(("my-spouse@example.com" "Love,")
                                  ("a-friend@example.com"
                                   ("Chau," "Ciao," "Cheers," "Later,"
                                    "Au revoir," "A bientot,"))
                                  ("#default" "Be seeing you,"))
  "An a list of closings")

(defvar xmlq-group-no-closing '()
  "These Gnus groups get no closing")

;; ================================================================================

(defvar xmlq-default-signature-function 'xmlq-generic-default-signature
  "What function should be called to generate a signature")

(defun xmlq-gnus-default-signature ()
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

(defun xmlq-generic-default-signature ()
  "This function just returns xmlq-message-signature-file"
  xmlq-message-signature-file)

(defun xmlq-default-signature ()
  (funcall xmlq-default-signature-function))

;; ================================================================================

(defvar xmlq-signature-override-function 'xmlq-nil-signature-override
  "What function should be called to override a quotation")

(defun xmlq-nil-signature-override (email quote)
  nil)

;; ================================================================================

(defun xmlq-add-mail-signature (&optional quotenum)
  "Inserts my signature and a mail quote"
  (interactive "P")
  (let* ((sigfile (xmlq-default-signature)))
    (xmlq-insert-mail-signature sigfile quotenum)))

(defun xmlq-insert-mail-signature (sigfile &optional quotenum)
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
      (insert (xmlq-mail-signature sigfile quotenum)))))

;; ================================================================================

(defun xmlq-gnus-insert-closing-hook ()
  "Closing hook for Gnus to insert the message closing"
  (let* ((group-name (if gnus-newsgroup-name
			 gnus-newsgroup-name
		       ""))
	 ;; don't downcase nil
	 (primary    (if (xmlq--mail-to-primary)
			 (downcase (xmlq--mail-to-primary))
		       ""))
	 (closing-text-data (if (assoc primary xmlq-closing-text-alist)
				(car (cdr
				      (assoc primary xmlq-closing-text-alist)))
			      (car (cdr
				    (assoc "#default"
					   xmlq-closing-text-alist)))))
	 (closing-text (if (listp closing-text-data)
			   (nth (random (length closing-text-data))
				closing-text-data)
			 closing-text-data)))
    (if (not (member group-name xmlq-group-no-closing))
	(progn
	  (goto-char (point-max))
	  (newline)
	  (insert (concat (make-string 40 ? ) closing-text "\n"))
	  (insert (concat (make-string 42 ? ) xmlq-closing-name))))))

;; ================================================================================

(defun xmlq-mail-signature (sigfile &optional quotenum)
  "Returns a formatted mail signature"
  (xmlq-formatted-signature sigfile (xmlq-signature-quote quotenum)))

(defun xmlq-formatted-signature (sig-file quote &optional line-length pad-lines)
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
		    (xmlq-block-formatted-signature sig-file quote line-length pad-lines)
		  (xmlq-line-formatted-signature sig-file quote line-length pad-lines)))
    (set-buffer-modified-p nil)
    (set-buffer this-buf)
    (kill-buffer scratch-buf)
    lines))

(defun xmlq-block-formatted-signature (sig-file quote &optional line-length pad-lines)
  "Format a quotation as a block, to the right of the |'s"
  (let ((lines ())
	(width 40)
	(sig-width 40)
	(fill-width line-length))
    (if (not fill-width)
	(setq fill-width 72))

    (setq sig-width (current-column))
    (setq width (- fill-width sig-width))
    (setq lines (xmlq--string-wrap quote width))
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

(defun xmlq-line-formatted-signature (sig-file quote &optional line-length pad-lines)
  "Format a quotation as a block, below the quotation, extending across the page"
  (let ((lines ())
	(width 72))
    (newline)
    (setq lines (xmlq--string-wrap quote width))
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

(defvar xmlq-signature-quote-number t)

(defun xmlq-next-signature-quote (emailaddr)
  "Return the next signature quote number for this person, if they appear in my bbdb. Otherwise return xmlq-signature-quote-number and increment it"
   (let ((qnum (random (xmlq-quote-count))))
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
							     (xmlq--increment-quote-number qnum))))
	     (if (numberp xmlq-signature-quote-number)
		 (setq qnum (xmlq--increment-quote-number xmlq-signature-quote-number))))))
     (setq xmlq-signature-quote-number qnum)))

(defun xmlq--increment-quote-number (qnum)
  "Increment quote number, but wrap around when we reach the end"
  (if (>= qnum (xmlq-quote-count))
      0
    (+ qnum 1)))

(defun xmlq-set-signature-quote (&optional prefixarg)
  (interactive "P")
  (if prefixarg
      (if (numberp prefixarg)
	  (setq xmlq-signature-quote-number prefixarg)
	(setq xmlq-signature-quote-number t)))
  (if (numberp xmlq-signature-quote-number)
      (message "Next signature quote is #%d" xmlq-signature-quote-number)
    (message "Next signature quote is random")))

(defun xmlq--mail-to-primary ()
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

(defun xmlq-signature-quote (&optional explicit-quote-num)
  (let* ((emailaddr (xmlq--mail-to-primary))
	 (quote-num (if explicit-quote-num
			explicit-quote-num
		      (xmlq-next-signature-quote emailaddr)))
	 quote-list quote)
    (setq quote-list (xmlq-quotation quote-num))
    (setq quote-num (car quote-list))

    (if (boundp 'xmlq-signature-override-function)
	(setq quote (funcall xmlq-signature-override-function emailaddr (cadr quote-list))))

    (if quote
	(message (format "Using override quotation for %s" emailaddr))
      (progn
	(setq quote (cadr quote-list))
	(message (format "Quotation #%d for %s" quote-num emailaddr))))

    quote))

;; ======================================================================

(defun xmlq--string-wrap (string &optional width)
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
