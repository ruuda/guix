;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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


(define-module (test-store)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix base32)
  #:use-module (gnu packages bootstrap)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64))

;; Test the (guix store) module.

(define %store
  (false-if-exception (open-connection)))

(when %store
  ;; Make sure we build everything by ourselves.
  (set-build-options %store #:use-substitutes? #f))

(define %seed
  (seed->random-state (logxor (getpid) (car (gettimeofday)))))

(define (random-text)
  (number->string (random (expt 2 256) %seed) 16))


(test-begin "store")

(test-skip (if %store 0 10))

(test-assert "dead-paths"
  (let ((p (add-text-to-store %store "random-text"
                              (random-text) '())))
    (member p (dead-paths %store))))

;; FIXME: Find a test for `live-paths'.
;;
;; (test-assert "temporary root is in live-paths"
;;   (let* ((p1 (add-text-to-store %store "random-text"
;;                                 (random-text) '()))
;;          (b  (add-text-to-store %store "link-builder"
;;                                 (format #f "echo ~a > $out" p1)
;;                                 '()))
;;          (d1 (derivation %store "link" (%current-system)
;;                          "/bin/sh" `("-e" ,b) '()
;;                          `((,b) (,p1))))
;;          (p2 (derivation-path->output-path d1)))
;;     (and (add-temp-root %store p2)
;;          (build-derivations %store (list d1))
;;          (valid-path? %store p1)
;;          (member (pk p2) (live-paths %store)))))

(test-assert "dead path can be explicitly collected"
  (let ((p (add-text-to-store %store "random-text"
                              (random-text) '())))
    (let-values (((paths freed) (delete-paths %store (list p))))
      (and (equal? paths (list p))
           (> freed 0)
           (not (file-exists? p))))))

(test-end "store")


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'test-assert 'scheme-indent-function 1)
;;; End:
