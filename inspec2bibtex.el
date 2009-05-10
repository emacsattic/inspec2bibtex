;; INSPEC to BibTeX bibliography conversion routine.
;; Copyright 1990 Ralph P. Sobek
;; inspect2bibtex.el  version 1.2
;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to ralph@laas.fr) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports, suggestions, or improvements to ralph@laas.fr.

(autoload 'bibtex-move-outside-of-entry "bibtex-mode")
(provide 'inspec2)

(defvar *INSPEC-msg-buffer* (get-buffer-create "*INSPEC Output*"))

(defvar *INSPEC-msg-flag* nil)

(defvar *INSPEC-buffer* nil)

(defvar *BibTeX-buffer* nil)

(defvar *INSPEC-last-id* "")

;; INSPEC REGEXPs

(defconst INSPEC-header
  "^Quest Accession Number : \\([0-9]+\\)\n  \\(.*\\)  INSPEC  \\(.*\\)  Issue.*$")

(defconst INSPEC-beginning "^  ")
(defconst INSPEC-end	"\\(^  \\|\\'\\)")

(defconst INSPEC-blank	"[ \n] *")
(defconst INSPEC-sep
  (concat "\n?,"
	  INSPEC-blank))

(defconst INSPEC-tail-sep
  (concat "\n?,\\("
	  INSPEC-blank
	  "\\|\\'\\)"))

(defconst INSPEC-tail	"Treatment: \\(.+\n\\)+\n")

(defconst INSPEC-year	"\\([0-9][0-9][0-9][0-9]\\)")

(defconst INSPEC-refs
  (concat "\\([0-9]+"
	  INSPEC-blank
	  "Refs\\)"))

(defconst INSPEC-ref-refs
  (concat INSPEC-refs
	  INSPEC-tail-sep))

(defconst INSPEC-day	"[0-9][0-9]?")

(defconst INSPEC-month
  "\\(Jan\\.\\|Feb\\.\\|March\\|April\\|May\\|June\\|July\\|Aug\\.\\|Sept\\.\\|Oct\\.\\|Nov\\.\\|Dec\\.\\)")

(defconst INSPEC-dates
  (concat "\\(\\(.*\\)"
	  INSPEC-blank
	  "\\)?"
	  INSPEC-year))

(defconst INSPEC-ref-dates
  (concat INSPEC-dates
	  INSPEC-tail-sep))

(defconst INSPEC-d-month-flag 1)
(defconst INSPEC-d-month-index 2)
(defconst INSPEC-d-year 3)

(defconst INSPEC-volume	"vol\\.\\([^ ,]+\\)")

(defconst INSPEC-ref-volume
  (concat INSPEC-volume
	  INSPEC-tail-sep))

(defconst INSPEC-pages	    "p\\.\n?\\([---0-9+]+\\)")

;; INSPEC Conference Paper REGEXPs

(defconst INSPEC-ref-pages
  (concat INSPEC-pages
	  "\\("
	  INSPEC-blank
	  "\\(pp\\|supl\\)\\.\\)?\\( "
	  INSPEC-volume
	  "\\)?"
	  INSPEC-tail-sep
	  ))

(defconst INSPEC-ref-pages-index 1)
(defconst INSPEC-ref-pp-index 2)
(defconst INSPEC-ref-volume-index 5)

(defconst INSPEC-country
  (concat "\\("
	  INSPEC-sep
	  ".*\\)*Country"
	  INSPEC-blank
	  "of"
	  INSPEC-blank
	  "Publ\\.:"
	  INSPEC-blank
	  "\\(.*\\)$"))

(defconst INSPEC-conference-address "\\(\\([^,]+, \\)?.*\\)  ")

(defconst INSPEC-conference-address&date
  (concat INSPEC-conference-address
	  "\\("
	  INSPEC-dates
	  "\\)$"))

(defconst INSPEC-c-address-index 1)
(defconst INSPEC-c-dates-index 3)
(defconst INSPEC-c-month-flag 4)
(defconst INSPEC-c-month-index 5)

;; INSPEC Journal Paper REGEXPs

(defconst INSPEC-journal-abbrev "\\(.+\\) (\\(.+\\))$")

(defconst INSPEC-journal-name
  (concat "\\(\\<[^.]+\\>\\)"
	  INSPEC-blank))

(defconst INSPEC-journal-number "no\\.\\([^ ,]+\\)")

(defconst INSPEC-ref-number
  (concat INSPEC-journal-number
	  INSPEC-tail-sep))

(defconst INSPEC-journal-reference-header
  (concat "\\("
	  INSPEC-journal-name
	  "\\)?"
	  INSPEC-volume))
  
(defconst INSPEC-j-name-flag 1)
(defconst INSPEC-j-name-index 2)

;; INSPEC Report REGEXPs

(defconst INSPEC-report "Rep No\\. \\([0-9A-Za-z---#.]+\\)")

(defconst INSPEC-report-info
  (concat INSPEC-report
	  "  \\([A-Z0-9][^,]*\\)"
	  INSPEC-sep
	  "\\(\\(.+\n\\)+\\)"
	  INSPEC-end))

;; INSPEC Match Functions

(defun match-quest ()
  "Function to match the next INSPEC quest."
  (interactive)
  (let (num type start end (last (point)) begin)
    (cond ((re-search-forward INSPEC-header nil 'skip 1)
	   (setq num (buffer-substring (match-beginning 2) (match-end 2)))
	   (setq type (buffer-substring (match-beginning 3) (match-end 3)))
	   (setq start (match-end 0))
	   (INSPEC-dump-junk last (match-beginning 0))
	   (re-search-forward paragraph-start)
	   (setq end (match-beginning 0))

	   (save-excursion
	     (save-restriction
	       (re-search-backward "^  Treatment:" start t 1)
	       (narrow-to-region start (match-beginning 0))
	       (goto-char start)
	       (cond ((string= "Conference Paper" type)
		      (match-conference-entry))
		     ((string= "Journal Paper" type)
		      (match-journal-entry))
		     ((string= "Report Section" type)
		      (match-in-collection-entry))
		     ((string= "Report" type)
		      (match-report-entry))
		     ((string= "Conference Proceedings" type)
		      (match-proceedings-entry))
		     ((string= "Book Chapter" type)
		      (match-in-book-entry))
		     (t (error "Unknown INSPEC type encountered: %s" type))))))
	  (t (INSPEC-dump-junk last (point-max))))
))

(defun match-conference-entry ()
  "Function to match INSPEC Conference Paper."
  (let ((title (c2string(match-field)))
	(authors (match-field))
	(authors-address (match-field))
	conference
	conference-ref publisher editors organization address dates volume
	annote journal-name number month year pages note
	assoc-value)


    (setq authors (squeeze (convert-names (car authors) (car (cdr authors)))))

    (re-search-forward INSPEC-end nil t 1)

    (cond ((match-conf-address&dates (point) (match-field-end))
	   (setq conference (c2string authors-address))
	   (setq authors-address nil)
	   (re-search-forward INSPEC-end nil t 1))
	  (t (setq conference (buffer-substring (point) (match-field-end)))
	     (re-search-forward INSPEC-end nil t 1)

	     (if (match-conf-address&dates (point) (match-field-end))
		 (re-search-forward INSPEC-end nil t 1)
	       (INSPEC-msg num
			   "Missing proceedings address or dates in \"%s\"."
			   (buffer-substring (point) (point-max))))))
		 

    (cond ((looking-at INSPEC-journal-abbrev)
	   (setq journal-abbrev (list (match-beginning 1) (match-end 1)))
	   (re-search-forward INSPEC-end nil t 1)

	   (if (match-INSPEC-reference-field (point) (match-field-end))
	       (re-search-forward INSPEC-end nil t 1)
	     (INSPEC-msg num "Missing journal citation in \"%s\"."
			 (buffer-substring (point) (point-max))))

	   (setq note (concat "In "
			      (squeeze conference)
			      ".  "
			      (squeeze dates)
			      ", "
			      (squeeze address)))

	   (match-language-field)
	   (match-sponsor-field)

	   (add-annotation num)

	   (add-entry "annote"	(squeeze annote))
	   (add-entry "note"	note)
	   (add-entry "organization"	(squeeze organization))
	   (add-entry "pages"	(squeeze pages))
	   (add-entry "number"	(squeeze number))
	   (add-entry "volume"	(squeeze volume))
	   (add-entry "month"	(squeeze month))
	   (add-entry "year"	year)
	   (add-entry "journal"	(squeeze (cond ((and (stringp journal-name)
						     (not (string= journal-name
								   "")))
						journal-name)
					       (t (c2string journal-abbrev)))))
	   (add-entry "title"	(squeeze title))
	   (add-entry "author"	authors)
	   (cons "Article" (cons (make-key authors) assoc-value)))
	  
	  ((match-INSPEC-reference-field (point) (match-field-end))
	   
	   (re-search-forward INSPEC-end nil t 1)
	   
	   (match-language-field)
	   (match-publisher-field t)
	   (match-pages-field)
	   (match-isbn-field)
	   (match-editors-field)
	   (match-sponsor-field)
	   
	   (add-annotation num)

	   (add-entry "annote"	(squeeze annote))
	   (add-entry "note"	note)
	   (add-entry "volume"	(squeeze volume))
	   (add-entry "address"	(squeeze address))
	   (add-entry "publisher"	(squeeze publisher))
	   (add-entry "organization"	(squeeze organization))
	   (add-entry "pages"	(squeeze pages))
	   (add-entry "editor"	(squeeze editors))
	   (add-entry "month"	(squeeze month))
	   (add-entry "year"	year)
	   (add-entry "booktitle"	(squeeze conference))
	   (add-entry "title"	(squeeze title))
	   (add-entry "author"	authors)
	   (cons "InProceedings" (cons (make-key authors) assoc-value)))
	  (t (INSPEC-msg num "Missing conference citation in \"%s\"."
			 (buffer-substring (point) (point-max))))))
  )

(defun match-journal-entry ()
  "Function to match INSPEC Journal Paper."
  (let ((title (c2string(match-field)))
	(authors (match-field))
	authors-address	journal-abbrev journal-name volume number
	month year pages note annote assoc-value)
    
    (re-search-forward INSPEC-end nil t 1)
    (cond ((not (looking-at INSPEC-journal-abbrev))
	   (setq authors-address (list (point) (match-field-end)))
	   (re-search-forward INSPEC-end nil t 1)
	   (cond ((not (looking-at INSPEC-journal-abbrev))
		  (INSPEC-msg num "Missing journal abbrev. in \"%s\"."
			      (buffer-substring (point) (point-max))))
		 (t
		  (setq journal-abbrev (list (match-beginning 1)
					     (match-end 1)))
		  (re-search-forward INSPEC-end nil t 1))))
	  
	  (t (setq journal-abbrev (list (match-beginning 1) (match-end 1)))
	   (re-search-forward INSPEC-end nil t 1)))
    
    (cond ((match-INSPEC-reference-field (point) (match-field-end))
	   (re-search-forward INSPEC-end nil t 1))
	  (t (INSPEC-msg num "Missing journal citation in \"%s\"."
			 (buffer-substring (point) (point-max)))))
    
    (match-language-field)
    
    (add-annotation num)
    
    (setq authors (squeeze (convert-names (car authors) (car (cdr authors)))))
    
    (add-entry "annote"	(squeeze annote))
    (add-entry "note"	note)
    (add-entry "pages"	(squeeze pages))
    (add-entry "number"	(squeeze number))
    (add-entry "volume"	(squeeze volume))
    (add-entry "month"	(squeeze month))
    (add-entry "year"	(squeeze year))
    (add-entry "journal"	(squeeze (cond ((and (stringp journal-name)
						     (not (string= journal-name
								   "")))
						journal-name)
					       (t (c2string journal-abbrev)))))
    (add-entry "title"	(squeeze title))
    (add-entry "author"	authors)
    (cons "Article" (cons (make-key authors) assoc-value))))

(defun match-proceedings-entry ()
  "Function to match INSPEC Conference Proceedings."
  (let ((title (c2string(match-field)))
	booktitle year volume number date publisher organization annote
	journal-name month pages note assoc-value)
    
    (re-search-forward INSPEC-end nil t 1)
    (if (match-conf-address&dates (point) (match-field-end))
	(re-search-forward INSPEC-end nil t 1)
      (INSPEC-msg num
		  "Missing proceedings address or dates in \"%s\"."
		  (buffer-substring (point) (point-max))))
		 

    (cond ((looking-at INSPEC-journal-abbrev)
	   (setq booktitle (buffer-substring (match-beginning 1) (match-end 1)))
	   (re-search-forward INSPEC-end nil t 1)))

    (cond ((match-INSPEC-reference-field (point) (match-field-end))
	   (re-search-forward INSPEC-end nil t 1))
	  (t (INSPEC-msg num "Missing proceedings citation in \"%s\"."
			 (buffer-substring (point) (+ (point) 30)))))
    
    (match-language-field)
    (match-publisher-field)
    (match-pages-field)
    (match-sponsor-field)
    
    (add-annotation num)
    
    (add-entry "annote"	(squeeze annote))
    ;;  (add-entry "note"	note)
    (add-entry "address"	(squeeze address))
    (add-entry "organization"	(squeeze organization))
    (add-entry "publisher"	(squeeze publisher))
    ;;  (add-entry "editor"	(squeeze editors))
    ;;  (add-entry "author"	(squeeze authors))
    (add-entry "month"	(squeeze month))
    (add-entry "year"	(squeeze year))
    (add-entry "booktitle"	(squeeze booktitle))
    (add-entry "title"	(squeeze title))
    (cons "Proceedings" (cons
			 (make-key (or organization publisher "Unknown"))
			 assoc-value)))
  )

(defun match-report-entry ()
  "Function to match INSPEC Report."
  (let ((title (c2string(match-field)))
	(authors (match-field))
	number institution address year month annote note journal-name volume
	assoc-value)
    
    (re-search-forward INSPEC-end nil t 1)

    (if (match-INSPEC-reference-field (point) (match-field-end))
	(re-search-forward INSPEC-end nil t 1)
      (INSPEC-msg num "Missing report date in \"%s\"."
			     (buffer-substring (point) (point-max))))

    (match-language-field)
    (match-pages-field)
    (cond ((looking-at INSPEC-report-info)
	   (setq number (buffer-substring (match-beginning 1) (match-end 1)))
	   (setq institution (buffer-substring (match-beginning 2)
					       (match-end 2)))
	   (setq address (buffer-substring (match-beginning 3) (match-end 3)))
	   (re-search-forward INSPEC-end nil t 1))
	  (t (INSPEC-msg num "Missing Report ref. in \"%s\"."
			 (buffer-substring (point) (point-max)))))
    
    
    (add-annotation num)
    
    (setq authors (squeeze (convert-names (car authors)	(car (cdr authors)))))
    
    (add-entry "annote"	(squeeze annote))
    (add-entry "note"	note)
    (add-entry "month"	(squeeze month))
    (add-entry "address"	(squeeze address))
    (add-entry "number"	(squeeze number))
    (add-entry "year"	year)
    (add-entry "institution"	(squeeze institution))
    (add-entry "title"	(squeeze title))
    (add-entry "author"	authors)
    (cons "TechReport" (cons (make-key authors) assoc-value)))
  )

(defun match-in-collection-entry ()
  "Function to match INSPEC Report Section."
  (let ((title (c2string(match-field)))
	(authors (match-field))
	(authors-address (match-field))
	booktitle note volume number date pages month year annote
	journal-name assoc-value)
    
    (re-search-forward INSPEC-end nil t 1)
    (cond ((not (match-INSPEC-reference-field (point) (match-field-end)))
	   (setq booktitle (buffer-substring (point) (match-field-end)))
	   (re-search-forward INSPEC-end nil t 1)

	   (if (match-INSPEC-reference-field (point) (match-field-end))
	       (re-search-forward INSPEC-end nil t 1)
	     (INSPEC-msg num "Missing in-collection citation in \"%s\"."
			 (buffer-substring (point) (point-max)))))

	  (t (setq booktitle (c2string authors-address))
	     (setq authors-address nil)
	     (re-search-forward INSPEC-end nil t 1)))
    
    (add-annotation num)
    
    (setq authors (squeeze (convert-names (car authors)	(car (cdr authors)))))
    
    (match-language-field)

    (add-entry "note"	note)
    (add-entry "annote"	(squeeze annote))
    ;;  (add-entry "publisher"	(squeeze publisher))
    ;;  (add-entry "editor"	(squeeze editor))
    (add-entry "year"	(squeeze year))
    (add-entry "booktitle"	(squeeze booktitle))
    (add-entry "pages"	(squeeze month))
    (add-entry "title"	(squeeze title))
    (add-entry "author"	authors)
    (cons "InCollection" (cons (make-key authors) assoc-value))))

(defun match-in-book-entry ()
  "Function to match INSPEC Book Chapter."
  (let ((title (c2string(match-field)))
	(authors (match-field))
	(authors-address (match-field))
	booktitle publisher editors year month pages note annote
	journal-name volume number assoc-value)
    
    (re-search-forward INSPEC-end nil t 1)
    (cond ((not (match-INSPEC-reference-field (point) (match-field-end)))
	   (setq booktitle (buffer-substring (point) (match-field-end)))
	   (re-search-forward INSPEC-end nil t 1)

	   (if (match-INSPEC-reference-field (point) (match-field-end))
	       (re-search-forward INSPEC-end nil t 1)
	     (INSPEC-msg num "Missing in-book citation in \"%s\"."
			 (buffer-substring (point) (point-max)))))

	  (t (setq booktitle (c2string authors-address))
	     (setq authors-address nil)
	     (re-search-forward INSPEC-end nil t 1)))
    
    (match-language-field)
    (match-publisher-field t)
    (match-pages-field)
    (match-isbn-field)
    (match-editors-field)
  
    (add-annotation num)
  
    (setq authors (squeeze (convert-names (car authors)	(car (cdr authors)))))
  
    (add-entry "annote"	(squeeze annote))
    (add-entry "note"	note)
    (add-entry "pages"	(squeeze pages))
    (add-entry "editor"	(squeeze editors))
    (add-entry "month"	(squeeze month))
    (add-entry "year"	year)
    (add-entry "publisher"	(squeeze publisher))
    (add-entry "booktitle"	(squeeze booktitle))
    (add-entry "title"	(squeeze title))
    (add-entry "author"	authors)
    (cons "InBook" (cons (make-key authors) assoc-value))))

(defun match-INSPEC-reference-field (start end)
  "Function to match the INSPEC reference field in all INSPEC documents."
  (interactive "r")
  (let (flag journal-flag)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(cond ((looking-at INSPEC-journal-reference-header)

	       ;; Journal Papers, Conference Papers, and Conference
	       ;; Proceedings may fall through this branch.
;;(/= start (match-beginning 0))
	       (cond (
		      (match-beginning INSPEC-j-name-flag)
		      (setq journal-name (buffer-substring
			     (match-beginning INSPEC-j-name-index)
			     (match-end INSPEC-j-name-index)))
;;			    (buffer-substring start (1- (match-beginning 0)))
		      (goto-char (match-end INSPEC-j-name-flag))))
	      
	       (cond ((looking-at INSPEC-ref-volume)
		      (setq volume (buffer-substring
				    (match-beginning 1) (match-end 1)))
		      (goto-char (match-end 0))))

	       (cond ((looking-at INSPEC-ref-number)
		      (setq number (buffer-substring (match-beginning 1)
						     (match-end 1)))
		      (goto-char (match-end 0))))
	       (setq flag t journal-flag t)))

	(cond ((looking-at INSPEC-ref-dates)

	       ;; Conference Papers, Conference Proceedings, Reports,
	       ;; Report Sections, Book Chapters, and also Journal
	       ;; Papers may fall through this branch.

	       (if (and (or (not month) journal-flag)
			(match-beginning INSPEC-d-month-flag)
			(/= (match-beginning INSPEC-d-month-index)
			    (match-end INSPEC-d-month-index)))
		   (setq month (buffer-substring
				(match-beginning INSPEC-d-month-index)
				(match-end INSPEC-d-month-index))))
	       (setq year (buffer-substring
			   (match-beginning INSPEC-d-year)
			   (match-end INSPEC-d-year)))
	       (goto-char (match-end 0))
	       (setq flag t)))

	(cond ((looking-at INSPEC-ref-pages)
	       (setq pages (buffer-substring
			    (match-beginning INSPEC-ref-pages-index)
			    (if (match-beginning INSPEC-ref-pp-index)
				(match-end INSPEC-ref-pp-index)
			      (match-end INSPEC-ref-pages-index))))
	       (if (match-beginning INSPEC-ref-volume-index)
		   (setq volume (buffer-substring
				 (match-beginning INSPEC-ref-volume-index)
				 (match-end INSPEC-ref-volume-index))))
	       (goto-char (match-end 0))
	       (setq flag t)))
	
	(cond ((looking-at INSPEC-ref-refs)
	       ;; flag is just a temporary variable here
	       (setq flag (buffer-substring (match-beginning 1)
						 (match-end 1)))
	       (if (/= 0 (string-to-int flag)) (add-annotation flag))
	       (setq flag t)))))
    (if flag (goto-char (1+ end)))
    flag))

(defun match-language-field ()
  (cond ((looking-at "\\(In .*\\)$")
	 (add-note (buffer-substring (match-beginning 1) (match-end 1)))
	 (re-search-forward INSPEC-end nil t 1))))

(defun match-publisher-field (&optional errorflag)
  (cond ((looking-at "Publisher: \\(.+\\)\\.")
	 (setq publisher (buffer-substring (match-beginning 1)
					   (match-end 1)))
	 (re-search-forward INSPEC-end nil t 1))
	(errorflag (INSPEC-msg num "Missing \"Publisher:\" field at \"%s\"."
			       (buffer-substring (point)
						 (min (+(point) 20)
						      (point-max)))))))

(defun match-pages-field ()
  (cond ((looking-at "Pages: ")
	 (re-search-forward INSPEC-end nil t 1))))

(defun match-isbn-field ()
  (cond ((looking-at "ISBN: ")
	 (re-search-forward INSPEC-end nil t 1))))

(defun match-editors-field ()
  (cond ((looking-at "\\(.*\\) (Editors)$")
	 (setq editors (convert-names (match-beginning 1)
				      (match-end 1)))
	 (re-search-forward INSPEC-end nil t 1))))

(defun match-sponsor-field ()
  (let (start)
    (cond ((looking-at "Sponsor: ")
	   (setq start (match-end 0))
	   (re-search-forward INSPEC-end nil t 1)
	   (setq organization (buffer-substring start
						(1- (match-beginning 0))))))))

(defun match-conf-address&dates (start end)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(cond ((looking-at INSPEC-conference-address&date)
		(setq address (buffer-substring
			       (match-beginning INSPEC-c-address-index)
			       (match-end INSPEC-c-address-index)))
		(setq dates (buffer-substring
			     (match-beginning INSPEC-c-dates-index)
			     (match-end INSPEC-c-dates-index)))

		(if (and (match-beginning INSPEC-c-month-flag)
			 (/= (match-beginning INSPEC-c-month-index)
			     (match-end INSPEC-c-month-index)))

		    (setq month (buffer-substring 
				 (match-beginning INSPEC-c-month-index)
					   (match-end INSPEC-c-month-index))))
		't)))))  
			  

(defun match-field-end ()
  (save-excursion (re-search-forward INSPEC-end nil t 1)
		  (1- (match-beginning 0))))

(defun match-field ()
  (re-search-forward INSPEC-beginning)
  (list (point)
	(match-field-end))
)

(defun convert-names (start end)
  "Function to convert list of names in INSPEC format (separated by
  `;'s) to BibTeX format (separated by ` and ').  The INSPEC names are
   in the buffer between START and END."
  (save-excursion
    (save-restriction
      (let (names)
	(narrow-to-region start end)
	(goto-char start)
	(while (and (/= (point) end)
		    (re-search-forward "\\([^;]*\\)\\(;\\|\\'\\)" end t 1))
	  (setq names (nconc names (list(list (match-beginning 1)
					      (match-end 1))))))
	(mapconcat (function(lambda (l) (buffer-substring (car l)
							  (car (cdr l)))))
		   names
		   " and "))))
)

(defun INSPEC-msg (num string &rest args)
  "Function to send an error message to the `*INSPEC-msg-buffer*'.
  The message consits of the INSPEC identity NUM, the formatting
  STRING and the ARGS to be formatted."
  (save-excursion
   (set-buffer *INSPEC-msg-buffer*)
   (if (string= num *INSPEC-last-id*)
       (insert-char ?  9)
     (setq *INSPEC-last-id* num)
     (insert (format "
In Ref. %s " num)))
   (insert (format string args) ?\n)
   (goto-char (point-max))))

(defun INSPEC-dump-junk (start end)
  "Function to check for extraneous junk and dump it to the
`*INSPEC-msg-buffer*'."
  (save-excursion
    (save-restriction
      (goto-char end)
      ;; There's an extra line before the INSPEC header
      (forward-line -1)
      (skip-chars-backward " \t\n" start)
      (if (/= start (point))
	  (progn
	    (setq end (point))
	    (goto-char start)
	    (skip-chars-forward " \t\n" end)
	    (beginning-of-line)
	    (if (/= end (point))
		(let ((flag *INSPEC-msg-flag*))
		  (narrow-to-region (point) end)
		  (set-buffer *INSPEC-msg-buffer*)
		  (or flag (progn
			     (insert
			      (format "%s:\n\n"
				      (buffer-file-name *INSPEC-buffer*)))
			     (goto-char (point-max))))
		  (insert-buffer *INSPEC-buffer*)
		  (goto-char (point-max))
		  (insert-char ?\n 2))))))))

(defun c2string (lst)
  "Function to convert a list of 2 buffer indices into a string."
  (buffer-substring (car lst) (car (cdr lst))))

(defun squeeze (string)
  "Function to squeeze all white-space in a string to individual
  spaces.  White-space at the beginnig or the end of the string is
  eliminated."
  (and string
       (let (start (oldpos 0) newstring (len (length string)))
	 (save-excursion
	   (while (setq start (string-match "[ \n]+" string start))
	     (cond ((= start 0))
		   ((= len (match-end 0))
		    (setq newstring (concat newstring
					    (substring string oldpos start))))
		   (t (setq newstring (concat newstring
					      (substring string oldpos start)
					      " "))))
	     (setq oldpos (setq start (match-end 0)))))
	 (if (/= oldpos len) (concat newstring (substring string oldpos))
	   newstring))))

(defun add-annotation (string)
  (setq annote (if (stringp annote)
		   (concat annote ", " string)
		 string)))

(defun add-note (string)
  (setq note (if (stringp note)
		   (concat note ", " string)
		 string)))

(defun add-entry (type entry)
  (if (and (stringp entry)
	   (/= 0 (length entry)))
      (setq assoc-value (cons (cons type entry) assoc-value))))

;; BibTex conversion functions

(defun make-key (string)
  "Function to create a BibTeX key based upon STRING and dynamic
  `year' of the form <string>-nn."
  (string-match "\\<\\([^ ,]+\\)\\>[, ]?" string)
  (concat (substring string (match-beginning 1) (match-end 1))
	  "-"
	  (if (and year (stringp year) (= (length year) 4))
	      (substring year 2)
	    "??")))

(defun make-bibtex (entry-type key assoc-list)
  "Function to create a filled BibTeX entry from the ENTRY-TYPE,
  author KEY, and association list, ASSOC-LIST, of BibTeX field keys
  and their associated values."
  (bibtex-move-outside-of-entry)
  (insert (concat "@" entry-type "{" key ",\n\n}\n\n"))
  (previous-line 3)
  (insert (mapconcat (function make-bibtex-entry) assoc-list ",\n"))
  (re-search-forward "}\n\n" nil t 1))

(defun make-bibtex-entry (assoc)
  "Function to build BibTeX entry field from ASSOC's field key and
  value."
  (concat "	" (car assoc) " = 	\"" (cdr assoc) "\""))

(defun convert-entry ()
  "Function to convert an INSPEC entry to its corresponding BibTeX
  entry."
  (let (quest)
    (setq quest (match-quest))
    (if (consp quest)
	(save-excursion
	  (set-buffer *BibTeX-buffer*)
	  (make-bibtex (car quest) (car (cdr quest)) (cdr (cdr quest)))))))

(defun inspec2bibtex (f1 f2)
  "Interactive function to convert an INSPEC file, F1, to the
  corresponding BibTeX file, F2."
  (interactive "fINSPEC file:\nFBibTeX file:")
  (find-file-read-only f1)
  (setq *INSPEC-buffer* (current-buffer))
  (setq *BibTeX-buffer* (find-file-noselect f2))
  (inspec2bibtex-region-1 (point-min) (point-max)))

(defun inspec2bibtex-region (start end f2)
  "Function to convert the region from START to END in the `*INSPEC
  buffer*' to BibTeX file F2."
  (interactive "r\nFBibTeX file:")
  (setq *INSPEC-buffer* (current-buffer))
  (setq *BibTeX-buffer* (find-file-noselect f2))
  (set-buffer *BibTeX-buffer*)
  (goto-char (point-max))
  (set-buffer *INSPEC-buffer*)
  (inspec2bibtex-region-1 start end))


(defun inspec2bibtex-region-1 (start end)
  (let ((max-msg (save-excursion (set-buffer *INSPEC-msg-buffer*)
				 (point-max))))
    (make-local-variable 'paragraph-start)
    (make-local-variable '*INSPEC-msg-flag*)
    (setq paragraph-start "^[ \f\n\t]*$")

    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(while (< (point) end)
	  (convert-entry))))
  
    (if (/= max-msg (save-excursion (set-buffer *INSPEC-msg-buffer*)
				 (point-max)))
	(pop-to-buffer *INSPEC-msg-buffer*))))
