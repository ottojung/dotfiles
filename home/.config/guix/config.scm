

(eval-when
 (expand load eval)
 (add-to-load-path (dirname (current-filename))))

(use-modules (gnu))
(use-modules (gnu packages))
(use-modules ((gnu packages freedesktop) :select (localed)))
(use-modules (gnu packages commencement))
(use-modules (guix profiles))
(use-modules (ice-9 match))
(use-modules (my helpers))

(use-package-modules haskell haskell-xyz wm gcc)

(define-packages cli-tools
  findutils ;; literally the "find" command
  diffutils ;; literally the "diff" command
  procps ;; commands like "ps", "top", "pkill", etc
  less ;; literally the "less" command
  ncurses ;; provides "clear" and "reset"
  dash ;; a POSIX-compliant shell
  time ;; measure time taken by programs
  pv ;; measures progress of the unix pipe
  man-db ;; default manual
  stow ;; a tool to deploy dotfiles
  ;; bc ;; very old calculator, provides bc and dc. Use awk instead
  fzf ;; command line fuzzy finder
  openssl ;; protocols & encryption stuff
  openssh ;; allows to ssh on servers
  recode ;; Charset converter tool and library
  jq ;; JSON query language. Preprocessor
  pup ;; HTML processor, like jq but for HTML instead of JSON
  rlwrap ;; provides `readline` functionality for CLI programs
  tree ;; like `ls` but recursive
  entr ;; runs command when file is updated
  ;; wget ;; downloads stuff from the internet, like curl
  ;; curl ;; downloads stuff from the internet, like wget
  libnotify ;; provides `notify-send'
  sox ;; provides `play' command
  figlet ;; transforms text into nice drawings
  cowsay ;; transforms text into nice drawings
  pamixer ;; program to control audio, setting volume and stuff, similar to `pactl'
  ;; doas ;; alternative to "sudo"
  ncdu ;; program for exploring filesystem
  brightnessctl ;; program to control screen brightnessctl
  dbus ;; for communications
  transmission ;; torrent server
  beep ;; makes sounds in the terminal
  file ;; provies information about files
  )

(define-packages cli-programs
  nvi ;; text editor "vi"
  git ;; version control system
  fish ;; shell
  htop ;; process monitor
  screen ;; terminal multiplexer a la tmux
  tmux ;; terminal multiplexer a la screen
  ;; cmus ;; terminal music player
  mpd mpd-mpc ncmpcpp ;; terminal music player suite
  ;; cordless ;; discord CLI client
  ;; neofetch ;; to look cool on the internet
  7zip ;; .7z archive manager
  unzip ;; .zip archive extractor
  zip ;; .zip archive creator
  ;; unrar ;; .rar archive manager, unfree :(
  ;; megacmd ;; mega.nz command line interface, unfree :(
  ;; ffmpeg ;; video covertery
  ;; pandoc ;; conversion between formats, like markdown to HTML
  ;; scc ;; cloc alternative
  cloc ;; scc alternative
  ;; nitrogen ;; changes wallpapers
  cpupower ;; `cpupower' utility (nix: linuxPackages.cpupower)
  lm-sensors ;; CPU diagnostics tool, provides `sensors' program and others (nix: lm_sensors)
  acpi ;; battery diagnostics tool
  rsync ;; copies files between remote computers
  unison ;; like rsync, but for syncing
  ;; youtube-dl ;; need newest version
  ;; texlive ;; distribution of TeX programs (nix: texlive.combined.scheme-full)
  imagemagick ;; CLI tool to edit images
  lynx ;; CLI browser
  weechat ;; CLI IRC client
  ;; docker docker-cli docker-compose ;; Docker stuff
  ;; podman ;; podman stuff. NOTE: does not work on Guix at the moment.
  restic ;; backup program with deduplication and incrementality.
  )

(define-packages compilers
  ;; gcc-toolchain ;; (nix: gcc)
  ;; binutils ;; gcc and linker and stuff
  ;; nodejs
  make ;; (nix: gnumake)
  ;; guile
  ;; python ;; (nix: python3)
  ;; racket
  ;; chez ;; provides `scheme' - r6rs compiler
  ;; chibi ;; provides `chibi-scheme' - r7rs interpreter
  ;; libck ;; instead of `cyclone-scheme` which is broken
  ;; dotnet-sdk ;; provides `dotnet` - C;;, F;; and VB.NET compilers
  ;; R libjpeg libpng ;; provides `R` - R-language interpreter
  ;; ghc ;; provides `ghci` - haskell language interpreter
  ;; coq ;; Coq language compiler
  ;; maxima ;; mathematica-like computer algebra system
  swi-prolog ;; free prolog implementation
  go ;; the golang compiler
  )

(define-packages x-server
  ;; xsel ;; copy-paste from terminal, alternative for xclip
  xclip ;; copy-paste from terminal, alternative for xsel
  ;; xmonad-with-packages ;; main xmonad program and libraries
  ;; xmobar ;; panel
  xdg-utils ;; provides xdg-open
  ;; mime-types ;; for xdg-open
  shared-mime-info ;; for xdg-open
  setxkbmap ;; for setting the language and swapping capslock
  arandr ;; managing multiple screens
  xrandr ;; managing resolution and stuff (nix: xorg.xrandr)
  xmodmap ;; managing shortcuts (nix: xorg.xmodmap)
  xev ;; captures keyboard keycodes (nix: xorg.xev)
  compton ;; allows transparency and screen sharing
  scrot ;; making screenshots
  ;; gromit-mpx ;; drawing on the screen ;; TODO: port to guix
  dunst ;; notify manager
  stalonetray ;; tray manager
  ;; trayer ;; tray manager ;; TODO: port to guix
  ;; xfce ;; desktop environment
  ;; slock ;; simple screen locking ;; broken? Says ~"cannot disable OOM"~ as if I wanted that.
  slim ;; provides `slimlock' screen locker
  xmessage ;; for displaying user messages
  awesome ;; the awesome window manager
  xdg-user-dirs ;; for xdg-user-dir command that is used by various programs
  )

(define-packages gui-small
  pcmanfm ;; gui file manager
  emacs ;; operating system
  ;; lilyterm ;; terminal emulator
  ;; mlterm ;; terminal emulator
  alacritty ;; terminal emulator
  rofi ;; application launcher
  mpv ;; simple video player
  pavucontrol ;; audio mixer/controller
  sxiv ;; simple X image viewer
  ;; transmission-remote-gtk ;; torrent client (nix: transmission-gtk)
  qbittorrent ;; torrent client
  ;; wicd ;; GUI wifi manager (does not work)
  )

(define-packages gui-big
  audacity ;; audio editor
  ;; telegram-desktop ;; Telegram desktop client (nix: tdesktop)
  ;; virtualbox ;; virtual machine (some parts are propriatary)
  virt-manager ;; virtual machine helper
  ;; ungoogled-chromium ;; chromium without google services
  ;; firefox ;; fuck mozilla
  ;; icecat ;; better firefox
  ;; torbrowser ;; breaks ;; TODO: track incoming: https://issues.guix.gnu.org/42380
  ;; nyxt ;; lisp-based web browser
  ;; qutebrowser ;; python-based web browser
  ;; teams ;; unfree :(
  obs ;; (nix: obs-studio)
  ;; flatpak
  okular ;; pdf-reader
  zathura ;; pdf-reader
  zathura-pdf-poppler ;; plugin for zathura that makes it actually work for pdfs
  vlc ;; video-player. Use mpv instead
  libreoffice
  ;; krita ;; paint program
  gimp ;; image editor
  ;; mypaint ;; simple paint program
  ;; element-desktop ;; default matrix client
  ;; openttd ;; game called Open Transport Tycoon Delux, economic simulator
  ;; cataclysm-dda ;; game called Cataclysm: Dark Days Ahead, zombie survival
  remmina ;; Remmina is a client to use other desktops remotely.  RDP, VNC, SPICE, NX, XDMCP, SSH and EXEC network protocols are supported.
  )

(define-packages fonts
  fontconfig
  font-google-noto
  font-google-noto-serif-cjk
  font-google-noto-sans-cjk
  font-gnu-unifont
  font-awesome
  ;; font-microsoft-web-core-fonts
  font-gnu-freefont
  font-dejavu
  font-liberation
  font-ghostscript
  font-jetbrains-mono
  font-wqy-microhei
  font-ibm-plex
  font-iosevka-aile
  font-iosevka
  font-inconsolata
  font-openmoji
  font-fira-code ;; (nix: fira-code)
  font-fira-go
  font-fira-sans
  font-fira-mono
  )

(define-packages e-mail
  notmuch ;; mail indexer
  isync ;; also known as `mbsync' and uses `~/.mbsyncrc'
  msmtp ;; for sending mails, uses `~/.msmtprc'
  ;; icedove ;; like "Thunderbird" email client
  icedove-minimal ;; smaller version of icedove
  )

(define-packages drivers
  xf86-input-wacom ;; wacom tablet driver
  libwacom ;; wacom tablet library
  ;; wacomtablet ;; KDE thingy
  )

(define-packages guix-packages
  gnupg ;; gpg for pgs
  pinentry ;; needed for GPG for some reason
  ;; nss-certs ;; HTTPS certificates
  glibc-locales ;; provides locales
  ;; glibc-utf8-locales-2.29 ;; provides utf8 locales
  ;; localed ;; provides systemd's localectl command
  )

(define-packages xmonad-packages
  ghc-xmonad-contrib xmonad xmobar)


(define base
  (patch-pkglist
   + cli-tools
   + cli-programs
   + compilers
   + x-server
   + gui-small
   + gui-big
   + fonts
   + e-mail
   + drivers
   + guix-packages
   + xmonad-packages))


(define marceline
  (patch-pkglist
   + base
   - openssh ;; weird DNS handling
   - man-db ;; missing lost of manual entries
   + ruby
   - texlive
   - x-server
   + arandr
   + ungoogled-chromium
   + go
   - firefox-esr
   - fonts
   - e-mail
   - drivers
   + alacritty
   - guix-packages
   + glibc-locales
   + nss-certs ;; HTTPS certificates
   - xmonad-packages))


(define to-install
  (let ((system (gethostname)))
    (match
     system
     ("marceline" marceline)
     (else base))))

(symbols->manifest to-install)
