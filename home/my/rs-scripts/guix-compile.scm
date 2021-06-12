#!/usr/bin/env guile
!#

(eval-when
 (load expand compile)
 (add-to-load-path
  (string-append (getenv "HOME") "/.config/guix")))

(use-modules (my helpers))

(fail-if-package-not-found? #t)

(define needed-for-compilation
  (list

   "make"
   "gcc-toolchain"
   "autoconf"
   "automake"
   "git"
   "binutils"
   "libtool"
   "flex"
   "gawk"
   "pkg-config"
   "libffi"
   "readline"
   "gettext"
   "gmp"
   "libfixposix"

   ;; For guile
   "libgc"
   "libunistring"
   "gperf"

   ;; ;; For Nyxt
   ;; "sbcl"
   ;; "xclip"
   ;; "glib-networking"
   ;; "webkitgtk"
   ;; "glib"
   ;; "gdk-pixbuf"
   ;; "cairo"
   ;; "pango"
   ;; "gsettings-desktop-schemas"
   ;; "xclip"
   ;; "gdk-pixbuf"
   ;; "cairo"
   ;; "pango"

   ;; Other
   "guile"
   "czmq"
   "cppzmq"
   "libevent"
   "jansson"
   "openssl"
   "curl"
   "gmp"
   "zlib"

   ))

;; Get only packages that exist
(define existing-packages
  (filter
   (compose filter-func list get-package+output)
   needed-for-compilation))

(define cmd
  (cons* "guix" "guix" "environment" "--ad-hoc"
         existing-packages))

(display "> ")
(for-each (lambda (w) (display w) (display " "))
          (cdr cmd))
(newline)

(apply execlp cmd)


