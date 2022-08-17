;;; lojban-hint.el --- interactive lojban hints

;; Copyright (c) 2003 Michele Bini

;; Author: Michele Bini
;; Maintainer: Michele Bini <mibin@libero.it>
;; Created: 12 Apr 2003
;; Version: 0.0
;; Keywords: lojban

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the license, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;;; Commentary:

;;; History:

(require 'lojban)

;;; Code:
(defcustom lojban-hint-warn-unrecognized t
  "Warn about unrecognized words rather then ignoring them."
  :group 'lojban-hint :type 'boolean)

(defun lojban-hint-brivla (brivla)
  (with-temp-buffer
    (insert brivla)
    (lojban-gloss-region (point-min) (point-max) t (current-buffer) t)
    (goto-char (point-min)) (forward-line 2)
    (buffer-substring
     (point) (save-excursion (end-of-line) (point)))))

(defun lojban-hint-gismu (gismu)
  (let ((pos (lojban-gismu-lookup gismu)))
    (and
     pos
     (let ((string
	    (save-window-excursion
	      (save-excursion
		(lojban-find-gismu-buffer)
		(goto-char (symbol-value pos))
		(buffer-substring
		 (+ (point) 13) (save-excursion (end-of-line)
						(point)))))))
       (let ((pos 0))
	 (while (string-match "[ \t\r\n][ \t\r\n]+" string nil)
	   (setq string (replace-match "; " nil t string)))
	 string)))))

(defun lojban-hint-cmavo (cmavo)
  (let ((pos (lojban-cmavo-lookup cmavo)))
    (and
     pos
     (let ((string
	    (save-window-excursion
	      (save-excursion
		(lojban-find-cmavo-buffer)
		(goto-char (symbol-value pos))
		(buffer-substring
		 (save-excursion
		   (beginning-of-line)
		   (let ((case-fold-search nil))
		     (re-search-forward "[A-Z]" nil t))
		   (match-beginning 0))
		 (save-excursion (end-of-line) (point)))))))
       (let ((pos 0))
	 (while (string-match "[ \t\r\n][ \t\r\n]+" string nil)
	   (setq string (replace-match "; " nil t string)))
	 string)))))

;; (lojban-hint-gismu "gismu")
;; (lojban-hint-cmavo "poi")

;;;###autoload
(defun lojban-hint-word-at-point (&optional word)
  "Gloss the lojban word at point."
  (interactive)
  (let ((type nil))
    (unless word
      (setq word
	    (save-excursion
	      (if (re-search-backward lojban-non-letter-rgx nil t)
		  (goto-char (match-end 0))
		(goto-char (point-min)))
	      (and (looking-at lojban-valsi-rgx)
		   (match-string 0))))
      (setq
       word
       (cond
	((not word) nil)
	((and
	  (string-match
	   (eval-when-compile
	     (concat "^" lojban-compound-cmavo-rgx "$"))
	   word)
	  (save-excursion
	    (and
	     (or (looking-at lojban-c-rgx)
		 (re-search-backward lojban-c-rgx nil t))
	     (looking-at lojban-cmavo-rgx))))
	 (setq type 'cmavo)
	 (match-string 0))
	((string-match lojban-brivla-rgx word)
	 (when lojban-hint-warn-unrecognized
	   ;; raise a verbose error if it turns out it is not
	   ;; morphologically valid
	   (lojban-brivla-p word t))
	 (setq type 'brivla)
	 (when (string-match lojban-gismu-rgx word)
	   (when lojban-hint-warn-unrecognized
	     (lojban-gismu-p word t))
	   (setq type 'gismu))
	 word)
	((string-match lojban-cmene-rgx word)
	 (when lojban-hint-warn-unrecognized
	   (lojban-cmene-p word t))
	 (setq type 'cmene)
	 word)
	(lojban-hint-warn-unrecognized
	 (error "Not a lojban word: %s" word))
	(t nil))))
    (when word
      (let ((meaning
	     (cond
	      ((eq type 'cmavo) (lojban-hint-cmavo word))
	      ((eq type 'gismu) (lojban-hint-gismu word))
	      ((eq type 'brivla) (lojban-hint-brivla word)))))
	(if meaning
	    (message "%s (%s) - %s" word (symbol-name type) meaning)
	  (message "%s (%s)" word (symbol-name type)))))))

;;;###autoload
(defun lojban-hint-word-under-mouse (event)
  (interactive "e")
  (let* ((pos (event-start event))
	 (window (posn-window pos))
	 (point (posn-point pos)))
    (with-current-buffer (window-buffer window)
      (save-excursion
	(goto-char point)
	(lojban-hint-word-at-point)))))

(provide 'lojban-hint)

;;; lojban-hint.el ends here
