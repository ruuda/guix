;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Coypright © 2016 ng0 <ng0@infotropique.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Alex Sassmannshausen <alex@pompo.co>
;;; Copyright © 2016, 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages perl-check)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages perl))

(define-public perl-test2-bundle-extended
  (package
    (name "perl-test2-bundle-extended")
    (version "0.000072")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/E/EX/EXODIST/Test2-Suite-"
                            version ".tar.gz"))
        (sha256
         (base32
          "0hgd6n29qjh1pwqvbglm2kb852yqshmixqqjhsr2kvvibdr58qpf"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda _ (setenv "PERL_USE_UNSAFE_INC" "1"))))))
    (propagated-inputs
     `(("perl-importer" ,perl-importer)
       ("perl-term-table" ,perl-term-table)
       ("perl-sub-info" ,perl-sub-info)))
    (home-page "http://search.cpan.org/~exodist/Test2-Suite/lib/Test2/Bundle/Extended.pm")
    (synopsis "Full set of tools for Test2::Suite")
    (description "This package provides a rich set of tools, plugins, bundles,
etc built upon the Test2 testing library.")
    (license perl-license)))

(define-public perl-test2-plugin-nowarnings
  (package
    (name "perl-test2-plugin-nowarnings")
    (version "0.06")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/Test2-Plugin-NoWarnings-"
                            version ".tar.gz"))
        (sha256
         (base32
          "002qk6qsm0l6r2kaxywvc38w0yf0mlavgywq8li076pn6kcw3242"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-test2-bundle-extended" ,perl-test2-bundle-extended)))
    (home-page "http://search.cpan.org/dist/Test2-Plugin-NoWarnings//")
    (synopsis "Fail if tests warn")
    (description "Loading this plugin causes your tests to fail if there any
warnings while they run.  Each warning generates a new failing test and the
warning content is outputted via diag.")
    (license perl-license)))

(define-public perl-test-base
  (package
    (name "perl-test-base")
    (version "0.88")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IN/INGY/"
                           "Test-Base-" version ".tar.gz"))
       (sha256
        (base32
         "0fch1cvivnszbnwhpfmwv1rin04j5xkj1n1ylfmlxg6bm72qqdjj"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-algorithm-diff" ,perl-algorithm-diff)
       ("perl-text-diff" ,perl-text-diff)))
    (propagated-inputs
     `(("perl-spiffy" ,perl-spiffy)
       ("perl-test-deep" ,perl-test-deep)))
    (home-page "http://search.cpan.org/dist/Test-Base/")
    (synopsis "Data-driven testing framework for Perl")
    (description "Test::Base gives a way to trivially write your own test
framework base class.  It concentrates on offering reusable data driven
patterns, so that you can write tests with a minimum of code.")
    (license perl-license)))

(define-public perl-test-class
  (package
    (name "perl-test-class")
    (version "0.50")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cpan.metacpan.org/authors/id/E/ET/ETHER/Test-Class-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0l0kk5jvxjkic2jkf1r7v41irb344aasnzr3f5ygjgxgiknm9489"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)))
    (inputs
     `(("perl-module-runtime" ,perl-module-runtime)
       ("perl-mro-compat" ,perl-mro-compat)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "http://search.cpan.org/dist/Test-Class/")
    (synopsis "Easily create test classes in an xUnit/JUnit style")
    (description "@code{Test::Class} provides a simple way of creating classes
and objects to test your code in an xUnit style.

Built using @code{Test::Builder}, it was designed to work with other
@code{Test::Builder} based modules (@code{Test::More},
@code{Test::Differences}, @code{Test::Exception}, etc.).")
    (license perl-license)))

(define-public perl-test-class-most
  (package
    (name "perl-test-class-most")
    (version "0.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/O/OV/OVID/Test-Class-Most-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1zvx9hil0mg0pnb8xfa4m0xgjpvh8s5gnbyprq3xwpdsdgcdwk33"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (inputs
     `(("perl-test-class" ,perl-test-class)
       ("perl-test-most" ,perl-test-most)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-try-tiny" ,perl-try-tiny)
       ("perl-mro-compat" ,perl-mro-compat)))
    (home-page "http://search.cpan.org/dist/Test-Class-Most/")
    (synopsis "Test classes the easy way")
    (description "@code{Test::Class::Most} provides some more convenience when
using @code{Test::Class}.")
    (license perl-license)))

(define-public perl-test-cleannamespaces
  (package
    (name "perl-test-cleannamespaces")
    (version "0.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Test-CleanNamespaces-" version ".tar.gz"))
       (sha256
        (base32
         "1jma95agqqy7iwdcl6jbg1waqz7mjqng4l046lpknhfxjhcj4al6"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-file-pushd" ,perl-file-pushd)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-warnings" ,perl-test-warnings)
       ("perl-test-tester" ,perl-test-tester)
       ("perl-test-needs" ,perl-test-needs)))
    (propagated-inputs
     `(("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-package-stash" ,perl-package-stash)
       ("perl-sub-identify" ,perl-sub-identify)
       ("perl-sub-exporter" ,perl-sub-exporter)
       ("perl-file-find-rule" ,perl-file-find-rule)
       ("perl-file-find-rule-perl" ,perl-file-find-rule-perl)))
    (home-page "http://search.cpan.org/dist/Test-CleanNamespaces/")
    (synopsis "Check for uncleaned imports")
    (description "This module lets you check your module's namespaces for
imported functions you might have forgotten to remove with
namespace::autoclean or namespace::clean and are therefore available to be
called as methods, which usually isn't want you want.")
    (license perl-license)))

(define-public perl-test-command
  (package
    (name "perl-test-command")
    (version "0.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/D/DA/DANBOO/Test-Command-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0cwm3c4d49mdrbm6vgh78b3x8mk730l0zg8i7xb9z8bkx9pzr8r8"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page "https://github.com/danboo/perl-test-command")
    (synopsis "Test routines for external commands")
    (description
     "This module provides routines for testing the exit status, standard
output and standard error of external commands.")
    (license perl-license)))

(define-public perl-test-cpan-meta
  (package
    (name "perl-test-cpan-meta")
    (version "0.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/B/BA/BARBIE/Test-CPAN-Meta-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1dcdbbdwdyhpldkhjzc9rvzlmb5jbil6fwh2x07nsfdwysf4ynzm"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-cpan-meta-json" ,perl-test-cpan-meta-json)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (home-page
     "http://search.cpan.org/dist/Test-CPAN-Meta/")
    (synopsis "Validate your CPAN META.yml files")
    (description
     "This module was written to ensure that a META.yml file meets the
specification.")
    (license artistic2.0)))

(define-public perl-test-cpan-meta-json
  (package
    (name "perl-test-cpan-meta-json")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/B/BA/BARBIE/Test-CPAN-Meta-JSON-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1jg9ka50ixwq083wd4k12rhdjq87w0ihb34gd8jjn7gvvyd51b37"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (inputs
     `(("perl-json" ,perl-json)))
    (home-page
     "http://search.cpan.org/dist/Test-CPAN-Meta-JSON/")
    (synopsis "Validate your CPAN META.json files")
    (description
     "This module was written to ensure that a META.json file meets the
specification.")
    (license artistic2.0)))

(define-public perl-test-deep
  (package
    (name "perl-test-deep")
    (version "1.120")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                                  "Test-Deep-" version ".tar.gz"))
              (sha256
               (base32
                "1kdy06r0yg7zwarqglc9163vbfb0sfc4s6ld4pw5q7i9f7mghzi0"))))
    (build-system perl-build-system)
    (inputs `(("perl-test-tester" ,perl-test-tester)
              ("perl-test-nowarnings" ,perl-test-nowarnings)))
    (synopsis "Flexible deep comparison for the Test::Builder framework")
    (description
     "Test::Deep compares two structures by going through each level, ensuring
that the values match, that arrays and hashes have the same elements and that
references are blessed into the correct class.  It also handles circular data
structures without getting caught in an infinite loop.")
    (home-page "http://search.cpan.org/dist/Test-Deep/")
    (license gpl1+)))  ; or "Artistic License"

(define-public perl-test-differences
  (package
    (name "perl-test-differences")
    (version "0.63")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DC/DCANTRELL/"
                           "Test-Differences-" version ".tar.gz"))
       (sha256
        (base32
         "0rhs4q6qn64ji06ns7lwl6iiiw3mggvd9xk9nkiqvx1jihbplrbw"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-text-diff" ,perl-text-diff)
       ("perl-capture-tiny" ,perl-capture-tiny)))
    (home-page "http://search.cpan.org/dist/Test-Differences/")
    (synopsis "Test strings and data structures and show differences")
    (description "This module exports three test functions and four diff-style
functions.")
    ;; See LICENSE section of Test/Differences.pm, which reads "... GNU public
    ;; license, any version, ..."
    (license gpl3+)))

(define-public perl-test-directory
  (package
    (name "perl-test-directory")
    (version "0.041")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SA/SANBEG/"
                           "Test-Directory-" version ".tar.gz"))
       (sha256
        (base32
         "1ncql08cizhicbxwd753b4czns8nlcnlw0zfjcfrbdd41x4j6hqr"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-test-exception" ,perl-test-exception)))
    (home-page "http://search.cpan.org/dist/Test-Directory/")
    (synopsis "Perl extension for maintaining test directories")
    (description "Testing code can involve making sure that files are created
and deleted as expected.  Doing this manually can be error prone, as it's easy
to forget a file, or miss that some unexpected file was added.  This module
simplifies maintaining test directories by tracking their status as they are
modified or tested with this API, making it simple to test both individual
files, as well as to verify that there are no missing or unknown files.")
    (license perl-license)))

(define-public perl-test-eol
  (package
    (name "perl-test-eol")
    (version "2.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/E/ET/ETHER/Test-EOL-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0l3bxpsw0x7j9nclizcp53mnf9wny25dmg2iglfhzgnk0xfpwzwf"))))
    (build-system perl-build-system)
    (home-page
     "http://search.cpan.org/dist/Test-EOL/")
    (synopsis
     "Check the correct line endings in your project")
    (description
     "@code{Test::EOL} lets you check for the presence of trailing whitespace
and/or windows line endings in your perl code.")
    (license perl-license)))

(define-public perl-test-exception
  (package
    (name "perl-test-exception")
    (version "0.43")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/EX/EXODIST/"
                           "Test-Exception-" version ".tar.gz"))
       (sha256
        (base32
         "0cxm7s4bg0xpxa6l6996a6iq3brr4j7p4hssnkc6dxv4fzq16sqm"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-sub-uplevel" ,perl-sub-uplevel)))
    (home-page "http://search.cpan.org/dist/Test-Exception/")
    (synopsis "Test exception based code")
    (description "This module provides a few convenience methods for testing
exception based code.  It is built with Test::Builder and plays happily with
Test::More and friends.")
    (license perl-license)))

(define-public perl-test-fatal
  (package
    (name "perl-test-fatal")
    (version "0.014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Test-Fatal-" version ".tar.gz"))
       (sha256
        (base32
         "1c6bs68mss4q7cyapkv2c0jn66i21050p0faxf3s3417gdffzp5w"))))
    (build-system perl-build-system)
    (propagated-inputs `(("perl-try-tiny" ,perl-try-tiny)))
    (home-page "http://search.cpan.org/dist/Test-Fatal/")
    (synopsis "Simple helpers for testing code with exceptions")
    (description "Test::Fatal is an alternative to the popular
Test::Exception.  It does much less, but should allow greater flexibility in
testing exception-throwing code with about the same amount of typing.")
    (license perl-license)))

(define-public perl-test-file-sharedir-dist
  (package
    (name "perl-test-file-sharedir-dist")
    (version "1.001002")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/K/KE/KENTNL/"
                            "Test-File-ShareDir-" version ".tar.gz"))
        (sha256
         (base32
          "1bbs6cx69wcinq77gif4i4pmrj8a7lwb92sgvvxzrwmjnk5lfdmk"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-class-tiny" ,perl-class-tiny)
       ("perl-file-copy-recursive" ,perl-file-copy-recursive)
       ("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-path-tiny" ,perl-path-tiny)
       ("perl-scope-guard" ,perl-scope-guard)
       ("perl-test-fatal" ,perl-test-fatal)))
    (home-page "https://github.com/kentnl/Test-File-ShareDir")
    (synopsis "Dist oriented ShareDir tester")
    (description "This module creates a Fake ShareDir for your modules
for testing.")
    (license perl-license)))

(define-public perl-test-files
  (package
    (name "perl-test-files")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/P/PH/PHILCROW/Test-Files-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1zn33yigznq7i1jr4yjr4lxvc6bn7znkbqdzj7slhc146pqapkln"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-algorithm-diff" ,perl-algorithm-diff)
       ("perl-text-diff" ,perl-text-diff)))
    (home-page "http://search.cpan.org/dist/Test-Files/")
    (synopsis "Ease software testing with files and directories")
    (description "This library provides functions to enable testing of files
and directories.  For instance, the @code{file_ok} helper can test whether the
contents of a file is equal to a particular string.")
    (license perl-license)))

(define-public perl-test-harness
  (package
    (name "perl-test-harness")
    (version "3.39")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "Test-Harness-" version ".tar.gz"))
       (sha256
        (base32
         "0chiqnzmna2mglm37nzxvn9qhq2j31iwz3i9isqjs7bf3k449gb9"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-test
           (lambda* (#:key inputs #:allow-other-keys)
             ;; This test looks for "#!/usr/bin/perl" in some source.
             ;; Patch what the test looks for.
             (substitute* "t/source.t"
               (("#!/usr/bin/perl")
                (string-append "#!" (assoc-ref inputs "perl")
                               "/bin/perl")))
             #t)))))
    (home-page "http://search.cpan.org/dist/Test-Harness/")
    (synopsis "Run Perl standard test scripts with statistics")
    (description "Simple test harness which allows tests to be run and results
automatically aggregated and output to STDOUT.")
    (license perl-license)))

(define-public perl-test-leaktrace
  (package
    (name "perl-test-leaktrace")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEEJO/"
                           "Test-LeakTrace-" version ".tar.gz"))
       (sha256
        (base32
         "00z4hcjra5nk700f3fgpy8fs036d7ry7glpn8g3wh7jzj7nrw22z"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Test-LeakTrace/")
    (synopsis "Traces memory leaks in Perl")
    (description "Test::LeakTrace provides several functions that trace memory
leaks.  This module scans arenas, the memory allocation system, so it can
detect any leaked SVs in given blocks.")
    (license perl-license)))

(define-public perl-test-longstring
  (package
    (name "perl-test-longstring")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RG/RGARCIA/"
                           "Test-LongString-" version ".tar.gz"))
       (sha256
        (base32
         "0kwp7rfr1i2amz4ckigkv13ah7jr30q6l5k4wk0vxl84myg39i5b"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Test-LongString/")
    (synopsis "Tests strings for equality, with more helpful failures")
    (description "This module provides some drop-in replacements for the
string comparison functions of Test::More, but which are more suitable when
you test against long strings.")
    (license perl-license)))

(define-public perl-test-manifest
  (package
    (name "perl-test-manifest")
    (version "2.02")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/B/BD/BDFOY/"
                                  "Test-Manifest-" version ".tar.gz"))
              (sha256
               (base32
                "15ik52l9macrrfizf4y6wj71d4lx7w590h2dfajnkmbxmz786iq6"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (home-page "http://search.cpan.org/dist/Test-Manifest/")
    (synopsis "Interact with a t/test_manifest file")
    (description "@code{Test::Manifest} overrides the default test file order.  Instead of
running all of the t/*.t files in ASCII-betical order, it looks in the t/test_manifest
file to find out which tests you want to run and the order in which you want to run them.
It constructs the right value for the build system to do the right thing.")
    (license perl-license)))

(define-public perl-test-memory-cycle
  (package
    (name "perl-test-memory-cycle")
    (version "1.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/P/PE/PETDANCE/Test-Memory-Cycle-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "00ijmgx1r3cxrcs1qa9rb2s4gbm3nsawd90drda89kb4r7yxslwx"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-padwalker" ,perl-padwalker)))
    (propagated-inputs
     `(("perl-devel-cycle" ,perl-devel-cycle)))
    (home-page
     "http://search.cpan.org/dist/Test-Memory-Cycle/")
    (synopsis
     "Verifies code hasn't left circular references")
    (description
     "@code{Test::Memory::Cycle} is built on top of @code{Devel::Cycle} to
give you an easy way to check for these circular references.

@example
use Test::Memory::Cycle;

my $object = new MyObject;
# Do stuff with the object.
memory_cycle_ok( $object );
@end example")
    (license artistic2.0)))

(define-public perl-test-mockobject
  (package
    (name "perl-test-mockobject")
    (version "1.20150527")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CH/CHROMATIC/"
                           "Test-MockObject-" version ".tar.gz"))
       (sha256
        (base32
         "160xvhbpwqjaff4fgckvldknldzcbn1z3jvyzybs7cqlj1x3bwdd"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)
       ("perl-test-warn" ,perl-test-warn)))
    (propagated-inputs
     `(("perl-test-exception" ,perl-test-exception)
       ("perl-test-warn" ,perl-test-warn)
       ("perl-universal-can" ,perl-universal-can)
       ("perl-universal-isa" ,perl-universal-isa)))
    (arguments `(#:tests? #f))          ;TODO: tests require perl-cgi
    (home-page "http://search.cpan.org/dist/Test-MockObject/")
    (synopsis "Emulate troublesome interfaces in Perl")
    (description "Test::MockObject allows you to create objects that conform
to particular interfaces with very little code.  You don't have to reimplement
the behavior, just the input and the output.")
    (license perl-license)))

(define-public perl-test-mocktime
  (package
    (name "perl-test-mocktime")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DD/DDICK/"
                           "Test-MockTime-" version ".tar.gz"))
       (sha256
        (base32
         "0j6cxmkj52i5xkwg8dg6klm0dh386fzc5v80n5nbdalpvq0h48c8"))))
    (propagated-inputs
     `(("perl-time-piece" ,perl-time-piece)))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Test-MockTime/")
    (synopsis "Replaces actual time with simulated time")
    (description "This module was created to enable test suites to test code
at specific points in time.  Specifically it overrides localtime, gmtime and
time at compile time and then relies on the user supplying a mock time via
set_relative_time, set_absolute_time or set_fixed_time to alter future calls
to gmtime,time or localtime.")
    (license perl-license)))

(define-public perl-test-most
  (package
    (name "perl-test-most")
    (version "0.35")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OV/OVID/"
                           "Test-Most-" version ".tar.gz"))
       (sha256
        (base32
         "0zv5dyzq55r28plffibcr7wd00abap0h2zh4s4p8snaiszsad5wq"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-test-differences" ,perl-test-differences)
       ("perl-test-warn" ,perl-test-warn)
       ("perl-exception-class" ,perl-exception-class)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-exception" ,perl-test-exception)))
    (home-page "http://search.cpan.org/dist/Test-Most/")
    (synopsis "Most commonly needed test functions and features")
    (description "This module provides the most commonly used testing
functions, along with automatically turning on strict and warning and gives a
bit more fine-grained control over test suites.")
    (license perl-license)))

(define-public perl-test-needs
  (package
    (name "perl-test-needs")
    (version "0.002005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/H/HA/HAARG/Test-Needs-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "16gkgpmr9hvkz382iaqd3500269lk2d44fqaw3dsrvc66nc36kss"))))
    (build-system perl-build-system)
    (home-page
     "http://search.cpan.org/dist/Test-Needs/")
    (synopsis
     "Skip tests when modules not available")
    (description "@code{Test::Needs} allows you to skip test scripts if
modules are not available.  The requested modules will be loaded, and
optionally have their versions checked.  If the module is missing, the test
script will be skipped.  Modules that are found but fail to compile will exit
with an error rather than skip.

If used in a subtest, the remainder of the subtest will be skipped.")
    (license perl-license)))

(define-public perl-test-notabs
  (package
    (name "perl-test-notabs")
    (version "2.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/E/ET/ETHER/Test-NoTabs-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "127kpl1va267qar2ia4c22xb96jby2jqnda3sj5pjgmxg8si26cg"))))
    (build-system perl-build-system)
    (home-page
     "http://search.cpan.org/dist/Test-NoTabs/")
    (synopsis
     "Check the presence of tabs in your project")
    (description
     "@code{Test::NoTabs} lets you check the presence of tabs in your perl
code.")
    (license perl-license)))

(define-public perl-test-nowarnings
  (package
    (name "perl-test-nowarnings")
    (version "1.04")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/A/AD/ADAMK/"
                                  "Test-NoWarnings-" version ".tar.gz"))
              (sha256
               (base32
                "0v385ch0hzz9naqwdw2az3zdqi15gka76pmiwlgsy6diiijmg2k3"))))
    (build-system perl-build-system)
    (inputs `(("perl-test-tester" ,perl-test-tester)))
    (synopsis "Ensure no warnings are produced while testing")
    (description
     "This modules causes any warnings during testing to be captured and
stored.  It automatically adds an extra test that will run when your script
ends to check that there were no warnings.  If there were any warnings, the
test will fail and output diagnostics of where, when and what the warning was,
including a stack trace of what was going on when it occurred.")
    (home-page (string-append "http://search.cpan.org/~adamk//"
                              "Test-NoWarnings-" version))
    (license lgpl2.1)))

(define-public perl-test-number-delta
  (package
    (name "perl-test-number-delta")
    (version "1.06")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                                  "Test-Number-Delta-" version ".tar.gz"))
              (sha256
               (base32
                "0jfhzhpzkc23mkrlbnv085ykpfncmy99hvppbzjnrpvgks8k0m2k"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Test-Number-Delta/")
    (synopsis
     "Compare the difference between numbers against a given tolerance")
    (description
     "At some point or another, most programmers find they need to compare
floating-point numbers for equality.  The typical idiom is to test if the
absolute value of the difference of the numbers is within a desired tolerance,
usually called epsilon.  This module provides such a function for use with
@code{Test::More}.")
    (license asl2.0)))

(define-public perl-test-output
  (package
    (name "perl-test-output")
    (version "1.03")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/B/BD/BDFOY/"
                                  "Test-Output-" version ".tar.gz"))
              (sha256
               (base32
                "12991jnzj4cbw9whhprmqvnzd1ayii84g2mh8vxbjngwqrjsy41i"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-test-tester" ,perl-test-tester)
       ("perl-sub-exporter" ,perl-sub-exporter)))
    (synopsis "Utilities to test STDOUT and STDERR messages")
    (description
     "Test::Output provides a simple interface for testing output sent to
STDOUT or STDERR.  A number of different utilities are included to try and be
as flexible as possible to the tester.")
    (home-page (string-append "http://search.cpan.org/~bdfoy//"
                              "Test-Output-" version))
    (license perl-license)))

(define-public perl-test-pod
  (package
    (name "perl-test-pod")
    (version "1.51")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Test-Pod-" version ".tar.gz"))
       (sha256
        (base32
         "1yvy5mc4j3s2h4aizryvark2nm58g2c6zhw9mlx9wmsavz7d78f1"))))
    (build-system perl-build-system)
    (native-inputs `(("perl-module-build" ,perl-module-build)))
    (home-page "http://search.cpan.org/dist/Test-Pod/")
    (synopsis "Check for POD errors in files")
    (description "Check POD files for errors or warnings in a test file, using
Pod::Simple to do the heavy lifting.")
    (license perl-license)))

(define-public perl-test-pod-coverage
  (package
    (name "perl-test-pod-coverage")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Test-Pod-Coverage-" version ".tar.gz"))
       (sha256
        (base32
         "1m203mhgfilz7iqc8mxaw4lw02fz391mni3n25sfx7nryylwrja8"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-pod-coverage" ,perl-pod-coverage)))
    (home-page "http://search.cpan.org/dist/Test-Pod-Coverage/")
    (synopsis "Check for pod coverage")
    (description "This module adds a test to your Perl distribution which
checks for pod coverage of all appropriate files.")
    (license artistic2.0)))

(define-public perl-test-requires
  (package
    (name "perl-test-requires")
    (version "0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TOKUHIROM/"
                           "Test-Requires-" version ".tar.gz"))
       (sha256
        (base32
         "1d9f481lj12cw1ciil46xq9nq16p6a90nm7yrsalpf8asn8s6s17"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Test-Requires/")
    (synopsis "Checks to see if the module can be loaded")
    (description "Test::Requires checks to see if the module can be loaded.
If this fails, then rather than failing tests this skips all tests.")
    (license perl-license)))

(define-public perl-test-script
  (package
    (name "perl-test-script")
    (version "1.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/P/PL/PLICEASE/"
                                  "Test-Script-" version ".tar.gz"))
              (sha256
               (base32
                "1msavbi6przkxq3npm90nv925v58iym9jrk677wn46x19whwzwzm"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-probe-perl" ,perl-probe-perl)))
    (synopsis "Basic cross-platform tests for scripts")
    (description
     "The intent of the Test::Script module is to provide a series of basic
tests for 80% of the testing you will need to do for scripts in the script (or
bin as is also commonly used) paths of your Perl distribution.")
    (home-page "http://search.cpan.org/dist/Test-Script/")
    (license perl-license)))

(define-public perl-test-sharedfork
  (package
    (name "perl-test-sharedfork")
    (version "0.29")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/EX/EXODIST/"
                           "Test-SharedFork-" version ".tar.gz"))
       (sha256
        (base32
         "0vlak10q4gcf0ch0rfcb9lvddav6r8h15iipzbkbgf9mrj47gbv3"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-requires" ,perl-test-requires)))
    (home-page "http://search.cpan.org/dist/Test-SharedFork/")
    (synopsis "Fork test in Perl")
    (description "Test::SharedFork is a utility module for Test::Builder.  It
makes fork(2) safe to use in test cases.")
    (license perl-license)))

(define-public perl-test-simple
  (package
    (name "perl-test-simple")
    (version "1.302120")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/E/EX/EXODIST/"
                                  "Test-Simple-" version ".tar.gz"))
              (sha256
               (base32
                "0v1l0hfza9zlw3qj5l2mrzljy1sk02h3yqcb4kixdb2d5l4n08y8"))))
    (build-system perl-build-system)
    (synopsis "Basic utilities for writing tests")
    (description
     "Test::Simple contains basic utilities for writing tests.")
    (home-page (string-append "http://search.cpan.org/~exodist//"
                              "Test-Simple-" version))
    (license perl-license)))

(define-public perl-test-taint
  (package
    (name "perl-test-taint")
    (version "1.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PETDANCE/Test-Taint-"
                           version ".tar.gz"))
       (sha256
        (base32
         "01rip5d7gdr1c7lq6yczzkqfd0500nfa977ryigylj6jj75526vj"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Test-Taint/")
    (synopsis "Checks for taintedness of variables")
    (description "Tainted data is data that comes from an unsafe source, such
as the command line, or, in the case of web apps, any @code{GET} or
@code{POST} transactions.  Read the @code{perlsec} man page for details on why
tainted data is bad, and how to untaint the data.

When you're writing unit tests for code that deals with tainted data, you'll
want to have a way to provide tainted data for your routines to handle, and
easy ways to check and report on the taintedness of your data, in standard
@code{Test::More} style.")
    (license perl-license)))

(define-public perl-test-tester
  (package
    (name "perl-test-tester")
    (version "0.109")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/F/FD/FDALY/"
                                  "Test-Tester-" version ".tar.gz"))
              (sha256
               (base32
                "0m9n28z09kq455r5nydj1bnr85lvmbfpcbjdkjfbpmfb5xgciiyk"))))
    (build-system perl-build-system)
    (synopsis "Simplify running Test::Builder tests")
    (description
     "Test::Tester allows testing of test modules based on Test::Builder with
a minimum of effort.")
    (home-page (string-append "http://search.cpan.org/~fdaly//"
                              "Test-Tester-" version))
    ;; "Under the same license as Perl itself"
    (license perl-license)))

(define-public perl-test-trap
  (package
    (name "perl-test-trap")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/EB/EBHANSSEN/"
                           "Test-Trap-v" version ".tar.gz"))
       (sha256
        (base32
         "1676gqjyk0zig3yyqv053y5j1pajp2af08ffmgx94n414whbhm5c"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-simple" ,perl-test-simple)))
    (propagated-inputs
     `(("perl-test-tester" ,perl-test-tester)
       ("perl-data-dump" ,perl-data-dump)))
    (home-page "http://search.cpan.org/dist/Test-Trap/")
    (synopsis "Trap exit codes, exceptions, output, and so on")
    (description "This module is primarily (but not exclusively) for use in
test scripts: A block eval configurable and extensible but by default trapping
STDOUT, STDERR, warnings, exceptions, would-be exit codes, and return values
from boxed blocks of test code.")
    (license perl-license)))

(define-public perl-test-utf8
  (package
    (name "perl-test-utf8")
    (version "1.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MA/MARKF/"
                           "Test-utf8-" version ".tar.gz"))
       (sha256
        (base32
         "0yhvf735v334qqvp9zg7i66qyk6r4cbk5s2psv93d3fdd4bindzg"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (home-page "http://search.cpan.org/dist/Test-utf8/")
    (synopsis "UTF-8 testing in Perl")
    (description "This module is a collection of tests useful for dealing with
UTF-8 strings in Perl.  This module has two types of tests: The validity tests
check if a string is valid and not corrupt, whereas the characteristics tests
will check that string has a given set of characteristics.")
    (license perl-license)))

(define-public perl-test-warn
  (package
    (name "perl-test-warn")
    (version "0.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CH/CHORNY/"
                           "Test-Warn-" version ".tar.gz"))
       (sha256
        (base32
         "0haf2ii7br5z0psmkvlvmx2z2q9qz1c70gx0969r378qjidmb5w1"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-sub-uplevel" ,perl-sub-uplevel)))
    (home-page "http://search.cpan.org/dist/Test-Warn/")
    (synopsis "Perl extension to test methods for warnings")
    (description "This module provides a few convenience methods for testing
warning based code.")
    (license perl-license)))

(define-public perl-test-warnings
  (package
    (name "perl-test-warnings")
    (version "0.026")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Test-Warnings-" version ".tar.gz"))
       (sha256
        (base32
         "024srkwjckp15dxkni9lb1hc8bg4xwc52zz0iich8rv1nnqnhaxf"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Test-Warnings/")
    (synopsis "Test for warnings and the lack of them")
    (description "This module is intended to be used as a drop-in replacement
for Test::NoWarnings.  It also adds an extra test, but runs this test before
done_testing calculates the test count, rather than after.  It does this by
hooking into done_testing as well as via an END block.  You can declare a
plan, or not, and things will still Just Work.")
    (license perl-license)))

(define-public perl-test-without-module
  (package
    (name "perl-test-without-module")
    (version "0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CO/CORION/"
                           "Test-Without-Module-" version ".tar.gz"))
       (sha256
        (base32
         "0zwc2dk5srd02j4p049w77m89iw5nbff381rmhcbaz8x2w5kdhz2"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Test-Without-Module/")
    (synopsis "Test fallback behaviour in absence of modules")
    (description "This module allows you to deliberately hide modules from a
program even though they are installed.  This is mostly useful for testing
modules that have a fallback when a certain dependency module is not
installed.")
    (license perl-license)))

(define-public perl-test-writevariants
  (package
    (name "perl-test-writevariants")
    (version "0.010")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "Test-WriteVariants-" version ".tar.gz"))
       (sha256
        (base32
         "0kklp05fj98yiq8znyfx9lx1vmjay2ypfb868qdwv3kf93m5zjwr"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-most" ,perl-test-most)
       ("perl-test-directory" ,perl-test-directory)))
    (propagated-inputs
     `(("perl-data-tumbler" ,perl-data-tumbler)
       ("perl-file-homedir" ,perl-file-homedir)
       ("perl-module-pluggable" ,perl-module-pluggable)))
    (home-page "http://search.cpan.org/dist/Test-WriteVariants/")
    (synopsis "Dynamic generation of tests")
    (description "The Test::WriteVariants module provides for the dynamic
generation of tests in nested combinations of contexts.")
    (license perl-license)))  ;See LICENSE

(define-public perl-test-yaml
  (package
    (name "perl-test-yaml")
    (version "1.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IN/INGY/"
                           "Test-YAML-" version ".tar.gz"))
       (sha256
        (base32
         "0hxrfs7p9hqkhvv5nhk2hd3kh32smwng4nz47b8xf4iw2q1n2dr7"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-test-base" ,perl-test-base)))
    (home-page "http://search.cpan.org/dist/Test-YAML/")
    (synopsis "Testing module for YAML implementations")
    (description "Test::YAML is a subclass of Test::Base with YAML specific
support.")
    (license perl-license)))

(define-public perl-test-trailingspace
 (package
  (name "perl-test-trailingspace")
  (version "0.0301")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/S/SH/SHLOMIF/Test-TrailingSpace-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0w2rvsksv7cmf80v632xm2rwxrv933kzz97839yhwynvg9s7b252"))))
  (build-system perl-build-system)
  (native-inputs
    `(("perl-module-build" ,perl-module-build)
      ("perl-file-find-object" ,perl-file-find-object)
      ("perl-class-xsaccessor" ,perl-class-xsaccessor)))
  (inputs
    `(("perl-file-find-object-rule" ,perl-file-find-object-rule)
      ("perl-text-glob" ,perl-text-glob)
      ("perl-number-compare" ,perl-number-compare)))
  (home-page
    "http://search.cpan.org/dist/Test-TrailingSpace/")
  (synopsis
    "Test for trailing space in Perl source files")
  (description "Test::TrailingSpace tests for trailing spaces
in Perl source files.")
  (license x11)))
