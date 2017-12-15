;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix man-db)
  #:use-module (guix zlib)
  #:use-module ((guix build utils) #:select (find-files))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:export (mandb-entry?
            mandb-entry-file-name
            mandb-entry-name
            mandb-entry-section
            mandb-entry-synopsis

            mandb-entries
            write-mandb-database))

;;; Comment:
;;;
;;; Scan gzipped man pages and create a man-db database.  The database is
;;; meant to be used by 'man -k KEYWORD'.
;;;
;;; The implementation here aims to be simpler than that of 'man-db', and to
;;; produce deterministic output.  See <https://bugs.gnu.org/29654>.
;;;
;;; Code:

;; Load 'gdbm-ffi' at run time to simplify the job of 'imported-modules' & co.
(module-use! (current-module) (resolve-interface '(gdbm)))

(define-record-type <mandb-entry>
  (mandb-entry file-name name section synopsis)
  mandb-entry?
  (file-name mandb-entry-file-name)               ;e.g., "../abiword.1.gz"
  (name      mandb-entry-name)                    ;e.g., "ABIWORD"
  (section   mandb-entry-section)                 ;number
  (synopsis  mandb-entry-synopsis))               ;string

(define (mandb-entry<? entry1 entry2)
  (match entry1
    (($ <mandb-entry> file1 name1 section1)
     (match entry2
       (($ <mandb-entry> file2 name2 section2)
        (or (< section1 section2)
            (string<? (basename file1) (basename file2))))))))

(define abbreviate-file-name
  (let ((man-file-rx (make-regexp "(.+)\\.[0-9][a-z]?(\\.gz)?$")))
    (lambda (file)
      (match (regexp-exec man-file-rx (basename file))
        (#f
         (basename file))
        (matches
         (match:substring matches 1))))))

(define (entry->string entry)
  "Return the wire format for ENTRY as a string."
  (match entry
    (($ <mandb-entry> file name section synopsis)
     (string-append (abbreviate-file-name file) "\t"
                    (number->string section) "\t"
                    (number->string section)

                    ;; Timestamps, that we always set to the epoch.
                    "\t0\t0"

                    ;; XXX: Weird things.
                    "\tB\t-\t-\tgz\t"

                    synopsis "\x00"))))

;; The man-db schema version we're compatible with.
(define %version-key "$version$\x00")
(define %version-value "2.5.0\x00")

(define (write-mandb-database file entries)
  "Write ENTRIES to FILE as a man-db database.  FILE is usually
\".../index.db\", and is a GDBM database."
  (let ((db (gdbm-open file GDBM_WRCREAT)))
    (gdbm-set! db %version-key %version-value)

    ;; Write ENTRIES in sorted order so we get deterministic output.
    (for-each (lambda (entry)
                (gdbm-set! db
                           (string-append (mandb-entry-file-name entry)
                                          "\x00")
                           (entry->string entry)))
              (sort entries mandb-entry<?))
    (gdbm-close db)))

(define (read-synopsis port)
  "Read from PORT a man page synopsis."
  (define (section? line)
    ;; True if LINE starts with ".SH", ".PP", or so.
    (string-prefix? "." (string-trim line)))

  (define (extract-synopsis str)
    (match (string-contains str "\\-")
      (#f "")
      (index
       (string-map (match-lambda
                     (#\newline #\space)
                     (chr chr))
                   (string-trim-both (string-drop str (+ 2 index)))))))

  ;; Synopses look like "Command \- Do something.", possibly spanning several
  ;; lines.
  (let loop ((lines '()))
    (match (read-line port 'concat)
      ((? eof-object?)
       (extract-synopsis (string-concatenate-reverse lines)))
      ((? section?)
       (extract-synopsis (string-concatenate-reverse lines)))
      (line
       (loop (cons line lines))))))

(define* (man-page->entry file #:optional (resolve identity))
  "Parse FILE, a gzipped man page, and return a <mandb-entry> for it."
  (define (string->number* str)
    (if (and (string-prefix? "\"" str)
             (> (string-length str) 1)
             (string-suffix? "\"" str))
        (string->number (string-drop (string-drop-right str 1) 1))
        (string->number str)))

  (call-with-gzip-input-port (open-file file "r0")
    (lambda (port)
      (let loop ((name     #f)
                 (section  #f)
                 (synopsis #f))
        (if (and name section synopsis)
            (mandb-entry file name section synopsis)
            (let ((line (read-line port)))
              (if (eof-object? line)
                  (mandb-entry file name (or section 0) (or synopsis ""))
                  (match (string-tokenize line)
                    ((".TH" name (= string->number* section) _ ...)
                     (loop name section synopsis))
                    ((".SH" (or "NAME" "\"NAME\""))
                     (loop name section (read-synopsis port)))
                    ((".so" link)
                     (match (and=> (resolve link)
                                   (cut man-page->entry <> resolve))
                       (#f
                        (loop name section synopsis))
                       (alias
                        (mandb-entry file
                                     (mandb-entry-name alias)
                                     (mandb-entry-section alias)
                                     (mandb-entry-synopsis alias)))))
                    (_
                     (loop name section synopsis))))))))))

(define (man-files directory)
  "Return the list of man pages found under DIRECTORY, recursively."
  (find-files directory "\\.[0-9][a-z]?(\\.gz)?$"))

(define (mandb-entries directory)
  "Return mandb entries for the man pages found under DIRECTORY, recursively."
  (map (lambda (file)
         (man-page->entry file
                          (lambda (link)
                            (let ((file (string-append directory "/" link
                                                       ".gz")))
                              (and (file-exists? file) file)))))
       (man-files directory)))
