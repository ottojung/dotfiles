#!/usr/bin/env guile
!#

(define packages
  (list
   "gnumake"
   "gcc"
   "autoconf"
   "automake"
   "git"
   "binutils"
   "libtool"
   "flex"
   "gawk"
   "pkgconfig"
   "libffi"
   "readline"
   "gettext"
   "gmp"
   "libfixposix"
   "zlib"

   ;; For guile
   "boehmgc"
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
   ;; "mime-types"
   ;; "pango"
   ;; "gtk3"
   ;; "gsettings-desktop-schemas"
   ;; "xclip"
   ;; "notify-osd"
   ;; "enchant2"
   ;; "lispPackages.log4cl"
   ;; "lispPackages.cffi"
   ;; "gst_all_1.gstreamer"
   ;; "gst_all_1.gst-libav"
   ;; "gst_all_1.gst-plugins-base"
   ;; "gst_all_1.gst-plugins-good"
   ;; "gst_all_1.gst-plugins-bad"
   ;; "gst_all_1.gst-plugins-ugly"
   ;; "gdk-pixbuf"
   ;; "cairo"
   ;; "mime-types"
   ;; "pango"
   ;; "gtk3"
   ;; "gsettings-desktop-schemas"

   ;; ;; Qt
   ;; "qt5.qtbase"
   ;; "qt5.qtwebchannel"
   ;; "qt5.qtwebengine"
   ;; "qt5.qtwebkit"
   ;; "qt5.qtdeclarative"

   ;; ;; Other
   ;; "guile"
   ;; "czmq"
   ;; "cppzmq"
   ;; "libevent"
   ;; "jansson"
   ;; "openssl"
   ;; "curlFull"
   ))

(apply execlp (cons* "nix-shell" "nix-shell" "--packages" packages))
