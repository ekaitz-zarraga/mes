;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018,2019,2020,2021,2022,2023,2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
;;;
;;; This file is part of GNU Mes.
;;;
;;; Also borrowing code from:
;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; GNU Mes is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Mes is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

(define-module (git mes)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mes)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define %source-dir (getcwd))

(define-public mescc-tools
  (package
    (name "mescc-tools")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/" name "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jak61gxab8bj8ddpgwfn9lqs917szq1phadmg8y5cjsndn1hv4k"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"
                         "armhf-linux" "aarch64-linux"
                         "riscv32-linux" "riscv64-linux"))
    (native-inputs (list which))
    (arguments
     (list
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      #:test-target "test"
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure))))
    (synopsis "Tools for the full source bootstrapping process")
    (description
     "Mescc-tools is a collection of tools for use in a full source
bootstrapping process.  It consists of the M1 macro assembler, the hex2
linker, the blood-elf symbol table generator, the kaem shell, exec_enable and
get_machine.")
    (home-page "https://savannah.nongnu.org/projects/mescc-tools")
    (license gpl3+)))

(define-public m2-planet
  (package
    (name "m2-planet")
    (version "1.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/oriansj/M2-Planet/releases/download/"
                    "Release_" version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c510p55amxjyvjlx9jpa30gixlgmf6mmfnaqcs46412krymwg38"))))
    (native-inputs (list mescc-tools))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"
                         "armhf-linux" "aarch64-linux"
                         "riscv32-linux" "riscv64-linux"))
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          (string-append "CC=" ,(cc-for-target)))
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'bootstrap)
                  (delete 'configure))))
    (synopsis "The PLAtform NEutral Transpiler")
    (description
     "M2-Planet, The PLAtform NEutral Transpiler, when combined with
mescc-tools compiles a subset of the C language into working binaries
with introspective steps inbetween.")
    (home-page "https://github.com/oriansj/m2-planet")
    (license gpl3+)))

(define-public nyacc-0.99
  (package
    (name "nyacc")
    (version "0.99.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hl5qxx19i4x1r0839sxm19ziqq65g4hy97yik81cc2yb9yvgyv3"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* (find-files "." "^Makefile\\.in$")
                    (("^SITE_SCM_DIR =.*")
                     "SITE_SCM_DIR = \
@prefix@/share/guile/site/@GUILE_EFFECTIVE_VERSION@\n")
                    (("^SITE_SCM_GO_DIR =.*")
                     "SITE_SCM_GO_DIR = \
@prefix@/lib/guile/@GUILE_EFFECTIVE_VERSION@/site-ccache\n")
                    (("^INFODIR =.*")
                     "INFODIR = @prefix@/share/info\n")
                    (("^DOCDIR =.*")
                     "DOCDIR = @prefix@/share/doc/$(PACKAGE_TARNAME)\n"))
                  #t))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list guile-2.2))
    (synopsis "LALR(1) Parser Generator in Guile")
    (description
     "NYACC is an LALR(1) parser generator implemented in Guile.
The syntax and nomenclature should be considered not stable.  It comes with
extensive examples, including parsers for the Javascript and C99 languages.")
    (home-page "https://savannah.nongnu.org/projects/nyacc")
    (license (list gpl3+ lgpl3+))))

(define-public nyacc-1.00.2
  (package
    (inherit nyacc-0.99)
    (version "1.00.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* (find-files "." "^Makefile\\.in$")
                    (("^SITE_SCM_DIR =.*")
                     "SITE_SCM_DIR = \
@prefix@/share/guile/site/@GUILE_EFFECTIVE_VERSION@\n")
                    (("^SITE_SCM_GO_DIR =.*")
                     "SITE_SCM_GO_DIR = \
@prefix@/lib/guile/@GUILE_EFFECTIVE_VERSION@/site-ccache\n")
                    (("^INFODIR =.*")
                     "INFODIR = @prefix@/share/info\n")
                    (("^DOCDIR =.*")
                     "DOCDIR = @prefix@/share/doc/$(PACKAGE_TARNAME)\n"))
                  #t))
              (sha256
               (base32
                "065ksalfllbdrzl12dz9d9dcxrv97wqxblslngsc6kajvnvlyvpk"))))
    (inputs (list guile-3.0))))

(define-public nyacc-1.02.0
  (package
    (inherit nyacc-0.99)
    (version "1.02.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1a0iffvi0h2y8rlkg9vkzgvc4afywmf3kkmjdnhq9r7m4crjpcyl"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "configure"
                    (("GUILE_GLOBAL_SITE=\\$prefix.*")
                     "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n"))
                  #t))))
    (inputs (list guile-3.0))))

(define-public nyacc-1.03.0
  (package
    (inherit nyacc-0.99)
    (version "1.03.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1vdiqpm3p0ndmpmkzcpkpjvgklfsk4wxrhkixdxbczpafdfl635p"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "configure"
                    (("GUILE_GLOBAL_SITE=\\$prefix.*")
                     "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n"))
                  #t))))
    (inputs (list guile-3.0))))

(define-public nyacc-1.04.0
  (package
    (inherit nyacc-0.99)
    (version "1.04.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0m3mcdaslvvr2iav8ga146hzsja2hdj4656pszljb4q2q7h25ip5"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "configure"
                    (("GUILE_GLOBAL_SITE=\\$prefix.*")
                     "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n"))
                  #t))))
    (inputs (list guile-3.0))))

(define-public nyacc-1.05.1
  (package
    (inherit nyacc-0.99)
    (version "1.05.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1ck3gyzln5dhamp317nv3waych12mczj05dm4wdblij6ab0l4863"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "configure"
                    (("GUILE_GLOBAL_SITE=\\$prefix.*")
                     "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n"))
                  #t))))
    (inputs (list guile-3.0))))

(define-public nyacc-1.06.5
  (package
    (inherit nyacc-0.99)
    (version "1.06.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1fbzz9bm4mkz4j40l2z02zjlbqj82dmv2ayz83zl3j8gj6z3lpdg"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "configure"
                  (("GUILE_GLOBAL_SITE=\\$prefix.*")
                   "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n")))))
    (inputs (list guile-3.0))))

(define-public nyacc-1.07.0
  (package
    (inherit nyacc-0.99)
    (version "1.07.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "01qb6h6bk684z8xda5d71q5f5l2z3q9jjz36if3jbpjc7b8dxjap"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "configure"
                  (("GUILE_GLOBAL_SITE=\\$prefix.*")
                   "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n")))))
    (inputs (list guile-3.0))))

(define-public nyacc-1.08.1
  (package
    (inherit nyacc-0.99)
    (version "1.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1vrz3pnlr3njwk6ksz85slcwawi8ngiqbw94wd9x3mgv85vsfmys"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "configure"
                  (("GUILE_GLOBAL_SITE=\\$prefix.*")
                   "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n")))))
    (inputs (list guile-3.0))))

(define-public nyacc-1.08.7
  (package
    (inherit nyacc-0.99)
    (version "1.08.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0kfkazwchlpbrcqin5c02adwzf9vn76x7ix3hq272dw21nczavii"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "configure"
                  (("GUILE_GLOBAL_SITE=\\$prefix.*")
                   "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n")))))
    (inputs (list guile-3.0))))

(define-public nyacc-1.09.0
  (package
    (inherit nyacc-0.99)
    (version "1.09.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0psbdpirz3b91qfk5as4rp7pf75940wsvpk2xfvvlbqcpicvn8p2"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "configure"
                  (("GUILE_GLOBAL_SITE=\\$prefix.*")
                   "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n")))))
    (inputs (list guile-3.0))))

(define-public nyacc-1.09.4
  (package
    (inherit nyacc-0.99)
    (version "1.09.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0h81sywxbkhwcx2gcax8ymag832aq51bxaqnnxcnpz0qx7a4crgf"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "configure"
                  (("GUILE_GLOBAL_SITE=\\$prefix.*")
                   "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n")))))
    (inputs (list guile-3.0))))

(define-public mes
  (package
    (name "mes")
    (version #!mes!# "0.26.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://ftp.gnu.org/pub/gnu/mes/mes-" version ".tar.gz"))
              (sha256
               (base32 #!mes!# "120a7ljnyi9127inaarlqv6cl98cmk72s3gmyad7cjrwy6cvzf1g"))))
    (build-system gnu-build-system)
    (supported-systems '("aarch64-linux" "armhf-linux" "i686-linux"
                         "x86_64-linux" "riscv64-linux"))
    (propagated-inputs (list mescc-tools nyacc-1.09.4))
    (native-inputs
     (append
      (list guile-3.0)
      (let ((target-system (or (%current-target-system)
                               (%current-system))))
        (cond
         ((string-prefix? "x86_64-linux" target-system)
          ;; Use cross-compiler rather than #:system "i686-linux" to get
          ;; MesCC 64 bit .go files installed ready for use with Guile.
          (list (cross-binutils "i686-unknown-linux-gnu")
                (cross-gcc "i686-unknown-linux-gnu")))
         ((string-prefix? "aarch64-linux" target-system)
          ;; Use cross-compiler rather than #:system "armhf-linux" to get
          ;; MesCC 64 bit .go files installed ready for use with Guile.
          (let ((triplet "arm-linux-gnueabihf"))
            (list (cross-binutils triplet) (cross-gcc triplet))))
         (else
          '())))
      (list graphviz help2man m2-planet
            perl                        ;build-aux/gitlog-to-changelog
            texinfo)))
    (arguments
     `(#:strip-binaries? #f)) ; binutil's strip b0rkes MesCC/M1/hex2 binaries
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib")))
           (search-path-specification
            (variable "MES_PREFIX")
            (separator #f)
            (files '("")))))
    (synopsis "Scheme interpreter and C compiler for full source bootstrapping")
    (description
     "GNU Mes--Maxwell Equations of Software--brings the Reduced Binary Seed
bootstrap to Guix and aims to help create full source bootstrapping for
GNU/Linux distributions.  It consists of a mutual self-hosting Scheme
interpreter in C and a Nyacc-based C compiler in Scheme and is compatible with
Guile.")
    (home-page "https://www.gnu.org/software/mes")
    (license gpl3+)))

(define-public mes.git
  (let ((version #!mes!# "0.26.2")
        (revision "0")
        (commit (read-string (open-pipe "git show HEAD | head -1 | cut -d ' ' -f 2" OPEN_READ))))
    (package
      (inherit mes)
      (name "mes.git")
      (version (string-append version "-" revision "." (string-take commit 7)))
      (source (local-file %source-dir
                          #:recursive? #t
                          #:select? (git-predicate %source-dir)))
      (native-inputs
       `(("gdb" ,gdb)
         ,@(package-native-inputs mes))))))
