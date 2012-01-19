;;; lbdb-complete.el --- Enable the use of external address books from within Emacs
;;
;; Copyright (C) 2012 Tom Schutter <t.schutter@comcast.net>.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;; Commentary:
;;
;; This package allows Emacs to use any of the external address book
;; access tools that already work with the Mutt MUA external query
;; mechanism.  http://www.mutt.org/doc/manual/manual-4.html#query
;;
;; Usage:
;;
;; (require 'lbdb-complete)
;;
;; Bind `lbdb-complete' to a key binding of your choice.
;; It will expand the text before point into an email address from
;; your external address book.

(require 'lbdb)

(defun lbdb-complete-bounds ()
  "Find text before point that should be used to search with."
  (let* ((end (point))
         (bol (save-excursion (beginning-of-line) (point)))
         (start (save-excursion (search-backward-regexp "[ :,]" bol t))))
    (if start (cons (1+ start) end) (cons bol end))))

(defun lbdb-complete-make-string (address)
  "Create a valid email address string from the given address."
  (if (null address) nil
    (apply 'format "%s <%s>" address)))

(defun lbdb-complete-completing-read (&rest args)
  "Call ido-completing-read if available."
  ;ido-completing-read is broken for me
  ;(if (fboundp 'ido-completing-read) (apply 'ido-completing-read args)
  ;  (apply 'completing-read args)))
  (apply 'completing-read args))

(defun lbdb-complete-single-result (results)
  "Narrow the results down to a result formatted with lbdb-complete-make-string."
  (cond
   ((null results) nil)
   ((= 1 (length results)) (lbdb-complete-make-string (car results)))
   (t (lbdb-complete-make-string (assoc (lbdb-complete-completing-read "Select Name: " results) results)))))

(defun lbdb-complete ()
  "Attempt to expand the text before point by querying lbdb.

This function is useful when bound to a key, for example, in Gnus
message mode.  When called, it will search the external address
book for entries that match the text behind point.  If a match is
found, that text will be replaced with the matching email
address."
  (interactive)
  (let* ((bounds (lbdb-complete-bounds))
         (query (and bounds (buffer-substring (car bounds) (cdr bounds))))
         (results (and query (lbdbq query)))
         (email (lbdb-complete-single-result results)))
    (if email
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert email))
      (message "No match."))))

(provide 'lbdb-complete)
