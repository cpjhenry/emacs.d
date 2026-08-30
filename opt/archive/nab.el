;;; nab.el --- Find links to Bible verses          -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  <mbork@mbork.pl>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The command `nab-open-place' reads the name of the Bible book and
;; the chapter number (also the verse number when called with a prefix
;; argument), opens the browser on the selected place in the New
;; American Bible and puts its URL on the top of the kill ring.  If
;; `org-stored-links' is bound, store the link also there.

;;; Code:

(setq nab-books
      '((:name "Genesis" :number 1 :link-label "genesis")
	(:name "Exodus" :number 2 :link-label "exodus")
	(:name "Leviticus" :number 3 :link-label "leviticus")
	(:name "Numbers" :number 4 :link-label "numbers")
	(:name "Deuteronomy" :number 5 :link-label "deuteronomy")
	(:name "Joshua" :number 6 :link-label "joshua")
	(:name "Judges" :number 7 :link-label "judges")
	(:name "Ruth" :number 8 :link-label "ruth")
	(:name "1 Samuel" :number 9 :link-label "1samuel")
	(:name "2 Samuel" :number 10 :link-label "2samuel")
	(:name "1 Kings" :number 11 :link-label "1kings")
	(:name "2 Kings" :number 12 :link-label "2kings")
	(:name "1 Chronicles" :number 13 :link-label "1chronicles")
	(:name "2 Chronicles" :number 14 :link-label "2chronicles")
	(:name "Ezra" :number 15 :link-label "ezra")
	(:name "Nehemiah" :number 16 :link-label "nehemiah")
	(:name "Tobit" :number 17 :link-label "tobit")
	(:name "Judith" :number 18 :link-label "judith")
	(:name "Esther" :number 19 :link-label "esther")
	(:name "1 Maccabees" :number 20 :link-label "1maccabees")
	(:name "2 Maccabees" :number 21 :link-label "2maccabees")
	(:name "Job" :number 22 :link-label "job")
	(:name "Psalms" :number 23 :link-label "psalms")
	(:name "Proverbs" :number 24 :link-label "proverbs")
	(:name "Ecclesiastes" :number 25 :link-label "ecclesiastes")
	(:name "Song of Songs" :number 26 :link-label "songofsongs")
	(:name "Wisdom" :number 27 :link-label "wisdom")
	(:name "Sirach" :number 28 :link-label "sirach")
	(:name "Isaiah" :number 29 :link-label "isaiah")
	(:name "Jeremiah" :number 30 :link-label "jeremiah")
	(:name "Lamentations" :number 31 :link-label "lamentations")
	(:name "Baruch" :number 32 :link-label "baruch")
	(:name "Ezekiel" :number 33 :link-label "ezekiel")
	(:name "Daniel" :number 34 :link-label "daniel")
	(:name "Hosea" :number 36 :link-label "hosea")
	(:name "Joel" :number 37 :link-label "joel")
	(:name "Amos" :number 38 :link-label "amos")
	(:name "Obadiah" :number 39 :link-label "obadiah")
	(:name "Jonah" :number 40 :link-label "jonah")
	(:name "Micah" :number 41 :link-label "micah")
	(:name "Nahum" :number 42 :link-label "nahum")
	(:name "Habakkuk" :number 43 :link-label "habakkuk")
	(:name "Zephaniah" :number 44 :link-label "zephaniah")
	(:name "Haggai" :number 45 :link-label "haggai")
	(:name "Zechariah" :number 46 :link-label "zechariah")
	(:name "Malachi" :number 47 :link-label "malachi")
	(:name "Matthew" :number 48 :link-label "matthew")
	(:name "Mark" :number 49 :link-label "mark")
	(:name "Luke" :number 50 :link-label "luke")
	(:name "John" :number 51 :link-label "john")
	(:name "Acts of the Apostles" :number 52 :link-label "acts")
	(:name "Romans" :number 53 :link-label "romans")
	(:name "1 Corinthians" :number 54 :link-label "1corinthians")
	(:name "2 Corinthians" :number 55 :link-label "2corinthians")
	(:name "Galatians" :number 56 :link-label "galatians")
	(:name "Ephesians" :number 57 :link-label "ephesians")
	(:name "Philippians" :number 58 :link-label "philippians")
	(:name "Colossians" :number 59 :link-label "colossians")
	(:name "1 Thessalonians" :number 60 :link-label "1thessalonians")
	(:name "2 Thessalonians" :number 61 :link-label "2thessalonians")
	(:name "1 Timothy" :number 62 :link-label "1timothy")
	(:name "2 Timothy" :number 63 :link-label "2timothy")
	(:name "Titus" :number 64 :link-label "titus")
	(:name "Philemon" :number 65 :link-label "philemon")
	(:name "Hebrews" :number 66 :link-label "hebrews")
	(:name "James" :number 67 :link-label "james")
	(:name "1 Peter" :number 68 :link-label "1peter")
	(:name "2 Peter" :number 69 :link-label "2peter")
	(:name "1 John" :number 70 :link-label "1john")
	(:name "2 John" :number 71 :link-label "2john")
	(:name "3 John" :number 72 :link-label "3john")
	(:name "Jude" :number 73 :link-label "jude")
	(:name "Revelation" :number 74 :link-label "revelation")))

(defvar nab-book-history ()
  "History of selected Bible books.")

(defun nab--read-book ()
  "Read the NAB book name (with completion)."
  (completing-read "Bible book: "
		   (mapcar (lambda (book)
			     (plist-get book
					:name))
			   nab-books)
		   nil t nil 'nab-book-history))

(defun nab--pad-number (number digits)
  "Pad NUMBER to DIGITS."
  (string-pad (number-to-string number) digits ?0 t))

(defun nab--generate-link (book chapter &optional verse)
  "Generate link to the suitable place in the New American Bible."
  (if verse
      (format "https://bible.usccb.org/bible/%s/%s#%s%s%s"
	      (plist-get book :link-label)
	      chapter
	      (nab--pad-number (plist-get book :number) 2)
	      (nab--pad-number chapter 3)
	      (nab--pad-number verse 3))
    (format "https://bible.usccb.org/bible/%s/%s"
	      (plist-get book :link-label)
	      chapter)))

(defun nab-open-place (book-name chapter &optional verse)
  "Open the given place in the New American Bible.
Ask the user for the book and chapter.  With a prefix argument,
ask also for a verse.  Leave the URL at the top of the kill ring."
  (interactive (list
		(nab--read-book)
		(read-number "Chapter number: ")
		(when current-prefix-arg
		  (read-number "Verse number: "))))
  (let ((url (nab--generate-link
	      (seq-find
	       (lambda (book-data)
		 (string= book-name (plist-get book-data :name)))
	       nab-books)
	      chapter
	      verse)))
    (kill-new url)
    (when (boundp 'org-stored-links)
      (push (list url
		  (if verse (format "%s %s:%s" book-name chapter verse)
		    (format "%s %s" book-name chapter)))
	    org-stored-links))
    (browse-url url)))

(provide 'nab)
;;; nab.el ends here
