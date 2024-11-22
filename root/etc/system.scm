;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu))
(use-modules ((gnu packages shells) #:select (dash)))
(use-modules ((gnu system setuid) #:select (setuid-program-program)))
(use-modules ((guix packages) #:select (package-name)))
(use-modules (nongnu packages linux)) ;; propriatary channels
(use-modules (srfi srfi-1))

(define this-hostname "pc")

(use-service-modules
  cups
  desktop
  networking
  virtualization
  ssh
  xorg)

(define (main)
  (operating-system
   ;; propriatary kernel + firmware
   (kernel linux)
   (firmware (list linux-firmware))

   ;; (locale "en_US.UTF-8")
   (timezone "US/Pacific")
   (keyboard-layout my-console-keyboard)
   (host-name this-hostname)
   (hosts-file this-hosts-file)

   ;; The list of user accounts ('root' is implicit).
   (users (cons* (user-account
                  (name "user1")
                  (comment "user1")
                  (group "user1g")
                  (home-directory "/home/user1")
                  (supplementary-groups
                   '("wheel" "netdev" "audio" "video" "libvirt" "lp" "tty" "input" "kvm" "docker")))
                 (user-account
                  (name "user2")
                  (comment "user2")
                  (group "user2g")
                  (home-directory "/home/user2")
                  (supplementary-groups
                   '("netdev" "audio" "video" "tty" "input")))
                 %base-user-accounts))
   (groups (cons* (user-group (name "user1g"))
                  (user-group (name "user2g"))
                  (user-group (name "docker"))
                  %base-groups))

   (packages
    (add-my-packages
     (remove-sudo-package
      %base-packages)))

   (setuid-programs
    (remove-sudo-setuid-program
     %setuid-programs))

   (services
    (add-my-desktop-services %desktop-services))

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
   (mapped-devices (list (mapped-device
                          (source (uuid
                                   "8b37a5aa-dd43-4f9c-901a-8e488ae6e701"))
                          (target "cryptroot")
                          (type luks-device-mapping))))

   ;; The list of file systems that get "mounted".  The unique
   ;; file system identifiers there ("UUIDs") can be obtained
   ;; by running 'blkid' in a terminal.
   (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "4ED6-B9A2"
                                       'fat32))
                         (type "vfat"))
                        (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "ext4")
                         (dependencies mapped-devices)) %base-file-systems))))


(define blocked-websites
  (list
   ;; youtube:
   "youtu.be"
   "www.youtu.be"
   "youtube.com"
   "www.youtube.com"
   "yewtu.be"
   "www.yewtu.be"
   "puffyan.us"
   "www.puffyan.us"
   "vid.puffyan.us"
   "invidious.snopyta.org"
   "www.snopyta.org"
   "kavin.rocks"
   "www.kavin.rocks"
   "invidious.kavin.rocks"
   "piped.kavin.rocks"
   "riverside.rocks"
   "www.riverside.rocks"
   "invidious.osi.kr"
   "www.invidious.osi.kr"
   "com.sb"
   "www.com.sb"
   "cthd.icu"
   "www.cthd.icu"
   "flokinet.to"
   "www.flokinet.to"
   "artemislena.eu"
   "www.artemislena.eu"
   "mutahar.rocks"
   "www.mutahar.rocks"
   "se...ivacy.com"
   "www.se...ivacy.com"
   "invidious.tiekoetter.com"
   "www.invidious.tiekoetter.com"
   "inv.bp.projectsegfau.lt"
   "www.inv.bp.projectsegfau.lt"
   "lunar.icu"
   "www.lunar.icu"
   "weblibre.org"
   "www.weblibre.org"
   "es...elbob.xyz"
   "www.es...elbob.xyz"
   "076.ne.jp"
   "www.076.ne.jp"
   "namazso.eu"
   "www.namazso.eu"
   "silkky.cloud"
   "www.silkky.cloud"
   "tokhmi.xyz"
   "www.tokhmi.xyz"
   "moomoo.me"
   "www.moomoo.me"
   "il.ax"
   "www.il.ax"
   "syncpundit.com"
   "www.syncpundit.com"
   "mha.fi"
   "www.mha.fi"
   "shimul.me"
   "www.shimul.me"
   "mint.lgbt"
   "www.mint.lgbt"
   "privacy.com.de"
   "www.privacy.com.de"
   "notyourcomputer.net"
   "www.notyourcomputer.net"
   "youtube.ac"
   "www.youtube.ac"
   "youtube.ad"
   "www.youtube.ad"
   "youtube.ae"
   "www.youtube.ae"
   "youtube.af"
   "www.youtube.af"
   "youtube.ag"
   "www.youtube.ag"
   "youtube.ai"
   "www.youtube.ai"
   "youtube.al"
   "www.youtube.al"
   "youtube.am"
   "www.youtube.am"
   "youtube.ao"
   "www.youtube.ao"
   "youtube.aq"
   "www.youtube.aq"
   "youtube.ar"
   "www.youtube.ar"
   "youtube.as"
   "www.youtube.as"
   "youtube.at"
   "www.youtube.at"
   "youtube.au"
   "www.youtube.au"
   "youtube.aw"
   "www.youtube.aw"
   "youtube.ax"
   "www.youtube.ax"
   "youtube.az"
   "www.youtube.az"
   "youtube.ba"
   "www.youtube.ba"
   "youtube.bb"
   "www.youtube.bb"
   "youtube.bd"
   "www.youtube.bd"
   "youtube.be"
   "www.youtube.be"
   "youtube.bf"
   "www.youtube.bf"
   "youtube.bg"
   "www.youtube.bg"
   "youtube.bh"
   "www.youtube.bh"
   "youtube.bi"
   "www.youtube.bi"
   "youtube.bj"
   "www.youtube.bj"
   "youtube.bm"
   "www.youtube.bm"
   "youtube.bn"
   "www.youtube.bn"
   "youtube.bo"
   "www.youtube.bo"
   "youtube.bq"
   "www.youtube.bq"
   "youtube.br"
   "www.youtube.br"
   "youtube.bs"
   "www.youtube.bs"
   "youtube.bt"
   "www.youtube.bt"
   "youtube.bw"
   "www.youtube.bw"
   "youtube.by"
   "www.youtube.by"
   "youtube.bz"
   "www.youtube.bz"
   "youtube.ca"
   "www.youtube.ca"
   "youtube.cc"
   "www.youtube.cc"
   "youtube.cd"
   "www.youtube.cd"
   "youtube.cf"
   "www.youtube.cf"
   "youtube.cg"
   "www.youtube.cg"
   "youtube.ch"
   "www.youtube.ch"
   "youtube.ci"
   "www.youtube.ci"
   "youtube.ck"
   "www.youtube.ck"
   "youtube.cl"
   "www.youtube.cl"
   "youtube.cm"
   "www.youtube.cm"
   "youtube.cn"
   "www.youtube.cn"
   "youtube.co"
   "www.youtube.co"
   "youtube.cr"
   "www.youtube.cr"
   "youtube.cu"
   "www.youtube.cu"
   "youtube.cv"
   "www.youtube.cv"
   "youtube.cw"
   "www.youtube.cw"
   "youtube.cx"
   "www.youtube.cx"
   "youtube.cy"
   "www.youtube.cy"
   "youtube.cz"
   "www.youtube.cz"
   "youtube.de"
   "www.youtube.de"
   "youtube.dj"
   "www.youtube.dj"
   "youtube.dk"
   "www.youtube.dk"
   "youtube.dm"
   "www.youtube.dm"
   "youtube.do"
   "www.youtube.do"
   "youtube.dz"
   "www.youtube.dz"
   "youtube.ec"
   "www.youtube.ec"
   "youtube.ee"
   "www.youtube.ee"
   "youtube.eg"
   "www.youtube.eg"
   "youtube.eh"
   "www.youtube.eh"
   "youtube.er"
   "www.youtube.er"
   "youtube.es"
   "www.youtube.es"
   "youtube.et"
   "www.youtube.et"
   "youtube.eu"
   "www.youtube.eu"
   "youtube.fi"
   "www.youtube.fi"
   "youtube.fj"
   "www.youtube.fj"
   "youtube.fk"
   "www.youtube.fk"
   "youtube.fm"
   "www.youtube.fm"
   "youtube.fo"
   "www.youtube.fo"
   "youtube.fr"
   "www.youtube.fr"
   "youtube.ga"
   "www.youtube.ga"
   "youtube.gd"
   "www.youtube.gd"
   "youtube.ge"
   "www.youtube.ge"
   "youtube.gf"
   "www.youtube.gf"
   "youtube.gg"
   "www.youtube.gg"
   "youtube.gh"
   "www.youtube.gh"
   "youtube.gi"
   "www.youtube.gi"
   "youtube.gl"
   "www.youtube.gl"
   "youtube.gm"
   "www.youtube.gm"
   "youtube.gn"
   "www.youtube.gn"
   "youtube.gp"
   "www.youtube.gp"
   "youtube.gq"
   "www.youtube.gq"
   "youtube.gr"
   "www.youtube.gr"
   "youtube.gs"
   "www.youtube.gs"
   "youtube.gt"
   "www.youtube.gt"
   "youtube.gu"
   "www.youtube.gu"
   "youtube.gw"
   "www.youtube.gw"
   "youtube.gy"
   "www.youtube.gy"
   "youtube.hk"
   "www.youtube.hk"
   "youtube.hm"
   "www.youtube.hm"
   "youtube.hn"
   "www.youtube.hn"
   "youtube.hr"
   "www.youtube.hr"
   "youtube.ht"
   "www.youtube.ht"
   "youtube.hu"
   "www.youtube.hu"
   "youtube.id"
   "www.youtube.id"
   "youtube.ie"
   "www.youtube.ie"
   "youtube.il"
   "www.youtube.il"
   "youtube.im"
   "www.youtube.im"
   "youtube.in"
   "www.youtube.in"
   "youtube.io"
   "www.youtube.io"
   "youtube.iq"
   "www.youtube.iq"
   "youtube.ir"
   "www.youtube.ir"
   "youtube.is"
   "www.youtube.is"
   "youtube.it"
   "www.youtube.it"
   "youtube.je"
   "www.youtube.je"
   "youtube.jm"
   "www.youtube.jm"
   "youtube.jo"
   "www.youtube.jo"
   "youtube.jp"
   "www.youtube.jp"
   "youtube.ke"
   "www.youtube.ke"
   "youtube.kg"
   "www.youtube.kg"
   "youtube.kh"
   "www.youtube.kh"
   "youtube.ki"
   "www.youtube.ki"
   "youtube.km"
   "www.youtube.km"
   "youtube.kn"
   "www.youtube.kn"
   "youtube.kp"
   "www.youtube.kp"
   "youtube.kr"
   "www.youtube.kr"
   "youtube.kw"
   "www.youtube.kw"
   "youtube.ky"
   "www.youtube.ky"
   "youtube.kz"
   "www.youtube.kz"
   "youtube.la"
   "www.youtube.la"
   "youtube.lb"
   "www.youtube.lb"
   "youtube.lc"
   "www.youtube.lc"
   "youtube.li"
   "www.youtube.li"
   "youtube.lk"
   "www.youtube.lk"
   "youtube.lr"
   "www.youtube.lr"
   "youtube.ls"
   "www.youtube.ls"
   "youtube.lt"
   "www.youtube.lt"
   "youtube.lu"
   "www.youtube.lu"
   "youtube.lv"
   "www.youtube.lv"
   "youtube.ly"
   "www.youtube.ly"
   "youtube.ma"
   "www.youtube.ma"
   "youtube.mc"
   "www.youtube.mc"
   "youtube.md"
   "www.youtube.md"
   "youtube.me"
   "www.youtube.me"
   "youtube.mg"
   "www.youtube.mg"
   "youtube.mh"
   "www.youtube.mh"
   "youtube.mk"
   "www.youtube.mk"
   "youtube.ml"
   "www.youtube.ml"
   "youtube.mm"
   "www.youtube.mm"
   "youtube.mn"
   "www.youtube.mn"
   "youtube.mo"
   "www.youtube.mo"
   "youtube.mp"
   "www.youtube.mp"
   "youtube.mq"
   "www.youtube.mq"
   "youtube.mr"
   "www.youtube.mr"
   "youtube.ms"
   "www.youtube.ms"
   "youtube.mt"
   "www.youtube.mt"
   "youtube.mu"
   "www.youtube.mu"
   "youtube.mv"
   "www.youtube.mv"
   "youtube.mw"
   "www.youtube.mw"
   "youtube.mx"
   "www.youtube.mx"
   "youtube.my"
   "www.youtube.my"
   "youtube.mz"
   "www.youtube.mz"
   "youtube.na"
   "www.youtube.na"
   "youtube.nc"
   "www.youtube.nc"
   "youtube.ne"
   "www.youtube.ne"
   "youtube.nf"
   "www.youtube.nf"
   "youtube.ng"
   "www.youtube.ng"
   "youtube.ni"
   "www.youtube.ni"
   "youtube.nl"
   "www.youtube.nl"
   "youtube.no"
   "www.youtube.no"
   "youtube.np"
   "www.youtube.np"
   "youtube.nr"
   "www.youtube.nr"
   "youtube.nu"
   "www.youtube.nu"
   "youtube.nz"
   "www.youtube.nz"
   "youtube.om"
   "www.youtube.om"
   "youtube.pa"
   "www.youtube.pa"
   "youtube.pe"
   "www.youtube.pe"
   "youtube.pf"
   "www.youtube.pf"
   "youtube.pg"
   "www.youtube.pg"
   "youtube.ph"
   "www.youtube.ph"
   "youtube.pk"
   "www.youtube.pk"
   "youtube.pl"
   "www.youtube.pl"
   "youtube.pm"
   "www.youtube.pm"
   "youtube.pn"
   "www.youtube.pn"
   "youtube.pr"
   "www.youtube.pr"
   "youtube.ps"
   "www.youtube.ps"
   "youtube.pt"
   "www.youtube.pt"
   "youtube.pw"
   "www.youtube.pw"
   "youtube.py"
   "www.youtube.py"
   "youtube.qa"
   "www.youtube.qa"
   "youtube.re"
   "www.youtube.re"
   "youtube.ro"
   "www.youtube.ro"
   "youtube.rs"
   "www.youtube.rs"
   "youtube.ru"
   "www.youtube.ru"
   "youtube.rw"
   "www.youtube.rw"
   "youtube.sa"
   "www.youtube.sa"
   "youtube.sb"
   "www.youtube.sb"
   "youtube.sc"
   "www.youtube.sc"
   "youtube.sd"
   "www.youtube.sd"
   "youtube.se"
   "www.youtube.se"
   "youtube.sg"
   "www.youtube.sg"
   "youtube.sh"
   "www.youtube.sh"
   "youtube.si"
   "www.youtube.si"
   "youtube.sk"
   "www.youtube.sk"
   "youtube.sl"
   "www.youtube.sl"
   "youtube.sm"
   "www.youtube.sm"
   "youtube.sn"
   "www.youtube.sn"
   "youtube.so"
   "www.youtube.so"
   "youtube.sr"
   "www.youtube.sr"
   "youtube.ss"
   "www.youtube.ss"
   "youtube.st"
   "www.youtube.st"
   "youtube.su"
   "www.youtube.su"
   "youtube.sv"
   "www.youtube.sv"
   "youtube.sx"
   "www.youtube.sx"
   "youtube.sy"
   "www.youtube.sy"
   "youtube.sz"
   "www.youtube.sz"
   "youtube.tc"
   "www.youtube.tc"
   "youtube.td"
   "www.youtube.td"
   "youtube.tf"
   "www.youtube.tf"
   "youtube.tg"
   "www.youtube.tg"
   "youtube.th"
   "www.youtube.th"
   "youtube.tj"
   "www.youtube.tj"
   "youtube.tk"
   "www.youtube.tk"
   "youtube.tl"
   "www.youtube.tl"
   "youtube.tm"
   "www.youtube.tm"
   "youtube.tn"
   "www.youtube.tn"
   "youtube.to"
   "www.youtube.to"
   "youtube.tr"
   "www.youtube.tr"
   "youtube.tt"
   "www.youtube.tt"
   "youtube.tv"
   "www.youtube.tv"
   "youtube.tw"
   "www.youtube.tw"
   "youtube.tz"
   "www.youtube.tz"
   "youtube.ua"
   "www.youtube.ua"
   "youtube.ug"
   "www.youtube.ug"
   "youtube.uk"
   "www.youtube.uk"
   "youtube.us"
   "www.youtube.us"
   "youtube.uy"
   "www.youtube.uy"
   "youtube.uz"
   "www.youtube.uz"
   "youtube.va"
   "www.youtube.va"
   "youtube.vc"
   "www.youtube.vc"
   "youtube.ve"
   "www.youtube.ve"
   "youtube.vg"
   "www.youtube.vg"
   "youtube.vi"
   "www.youtube.vi"
   "youtube.vn"
   "www.youtube.vn"
   "youtube.vu"
   "www.youtube.vu"
   "youtube.wf"
   "www.youtube.wf"
   "youtube.ws"
   "www.youtube.ws"
   "youtube.ye"
   "www.youtube.ye"
   "youtube.yt"
   "www.youtube.yt"
   "youtube.za"
   "www.youtube.za"
   "youtube.zm"
   "www.youtube.zm"
   "youtube.zw"
   "www.youtube.zw"
   "youtube.xn--lgbbat1ad8j"
   "www.youtube.xn--lgbbat1ad8j"
   "youtube.xn--y9a3aq"
   "www.youtube.xn--y9a3aq"
   "youtube.xn--mgbcpq6gpa1a"
   "www.youtube.xn--mgbcpq6gpa1a"
   "youtube.xn--54b7fta0cc"
   "www.youtube.xn--54b7fta0cc"
   "youtube.xn--90ais"
   "www.youtube.xn--90ais"
   "youtube.xn--90ae"
   "www.youtube.xn--90ae"
   "youtube.xn--fiqs8s"
   "www.youtube.xn--fiqs8s"
   "youtube.xn--fiqz9s"
   "www.youtube.xn--fiqz9s"
   "youtube.xn--wgbh1c"
   "www.youtube.xn--wgbh1c"
   "youtube.xn--e1a4c"
   "www.youtube.xn--e1a4c"
   "youtube.xn--qxa6a"
   "www.youtube.xn--qxa6a"
   "youtube.xn--node"
   "www.youtube.xn--node"
   "youtube.xn--qxam"
   "www.youtube.xn--qxam"
   "youtube.xn--j6w193g"
   "www.youtube.xn--j6w193g"
   "youtube.xn--h2brj9c"
   "www.youtube.xn--h2brj9c"
   "youtube.xn--mgbbh1a71e"
   "www.youtube.xn--mgbbh1a71e"
   "youtube.xn--fpcrj9c3d"
   "www.youtube.xn--fpcrj9c3d"
   "youtube.xn--gecrj9c"
   "www.youtube.xn--gecrj9c"
   "youtube.xn--s9brj9c"
   "www.youtube.xn--s9brj9c"
   "youtube.xn--xkc2dl3a5ee0h"
   "www.youtube.xn--xkc2dl3a5ee0h"
   "youtube.xn--45brj9c"
   "www.youtube.xn--45brj9c"
   "youtube.xn--2scrj9c"
   "www.youtube.xn--2scrj9c"
   "youtube.xn--rvc1e0am3e"
   "www.youtube.xn--rvc1e0am3e"
   "youtube.xn--45br5cyl"
   "www.youtube.xn--45br5cyl"
   "youtube.xn--3hcrj9c"
   "www.youtube.xn--3hcrj9c"
   "youtube.xn--mgbbh1a"
   "www.youtube.xn--mgbbh1a"
   "youtube.xn--h2breg3eve"
   "www.youtube.xn--h2breg3eve"
   "youtube.xn--h2brj9c8c"
   "www.youtube.xn--h2brj9c8c"
   "youtube.xn--mgbgu82a"
   "www.youtube.xn--mgbgu82a"
   "youtube.xn--mgba3a4f16a"
   "www.youtube.xn--mgba3a4f16a"
   "youtube.xn--mgbtx2b"
   "www.youtube.xn--mgbtx2b"
   "youtube.xn--4dbrk0ce"
   "www.youtube.xn--4dbrk0ce"
   "youtube.xn--mgbayh7gpa"
   "www.youtube.xn--mgbayh7gpa"
   "youtube.xn--80ao21a"
   "www.youtube.xn--80ao21a"
   "youtube.xn--q7ce6a"
   "www.youtube.xn--q7ce6a"
   "youtube.xn--mix082f"
   "www.youtube.xn--mix082f"
   "youtube.xn--mix891f"
   "www.youtube.xn--mix891f"
   "youtube.xn--mgbx4cd0ab"
   "www.youtube.xn--mgbx4cd0ab"
   "youtube.xn--mgbah1a3hjkrd"
   "www.youtube.xn--mgbah1a3hjkrd"
   "youtube.xn--l1acc"
   "www.youtube.xn--l1acc"
   "youtube.xn--mgbc0a9azcg"
   "www.youtube.xn--mgbc0a9azcg"
   "youtube.xn--d1alf"
   "www.youtube.xn--d1alf"
   "youtube.xn--mgb9awbf"
   "www.youtube.xn--mgb9awbf"
   "youtube.xn--mgbai9azgqp6j"
   "www.youtube.xn--mgbai9azgqp6j"
   "youtube.xn--ygbi2ammx"
   "www.youtube.xn--ygbi2ammx"
   "youtube.xn--wgbl6a"
   "www.youtube.xn--wgbl6a"
   "youtube.xn--p1ai"
   "www.youtube.xn--p1ai"
   "youtube.xn--mgberp4a5d4ar"
   "www.youtube.xn--mgberp4a5d4ar"
   "youtube.xn--90a3ac"
   "www.youtube.xn--90a3ac"
   "youtube.xn--yfro4i67o"
   "www.youtube.xn--yfro4i67o"
   "youtube.xn--clchc0ea0b2g2a9gcd"
   "www.youtube.xn--clchc0ea0b2g2a9gcd"
   "youtube.xn--3e0b707e"
   "www.youtube.xn--3e0b707e"
   "youtube.xn--fzc2c9e2c"
   "www.youtube.xn--fzc2c9e2c"
   "youtube.xn--xkc2al3hye2a"
   "www.youtube.xn--xkc2al3hye2a"
   "youtube.xn--mgbpl2fh"
   "www.youtube.xn--mgbpl2fh"
   "youtube.xn--ogbpf8fl"
   "www.youtube.xn--ogbpf8fl"
   "youtube.xn--kprw13d"
   "www.youtube.xn--kprw13d"
   "youtube.xn--kpry57d"
   "www.youtube.xn--kpry57d"
   "youtube.xn--o3cw4h"
   "www.youtube.xn--o3cw4h"
   "youtube.xn--pgbs0dh"
   "www.youtube.xn--pgbs0dh"
   "youtube.xn--j1amh"
   "www.youtube.xn--j1amh"
   "youtube.xn--mgbaam7a8h"
   "www.youtube.xn--mgbaam7a8h"
   "youtube.xn--mgb2ddes"
   "www.youtube.xn--mgb2ddes"
   "youtube.ad"
   "www.youtube.ad"
   "youtube.as"
   "www.youtube.as"
   "youtube.az"
   "www.youtube.az"
   "youtube.bz"
   "www.youtube.bz"
   "youtube.cc"
   "www.youtube.cc"
   "youtube.cd"
   "www.youtube.cd"
   "youtube.co"
   "www.youtube.co"
   "youtube.dj"
   "www.youtube.dj"
   "youtube.fm"
   "www.youtube.fm"
   "youtube.gg"
   "www.youtube.gg"
   "youtube.io"
   "www.youtube.io"
   "youtube.la"
   "www.youtube.la"
   "youtube.me"
   "www.youtube.me"
   "youtube.ms"
   "www.youtube.ms"
   "youtube.nu"
   "www.youtube.nu"
   "youtube.NL"
   "www.youtube.NL"
   "youtube.sc"
   "www.youtube.sc"
   "youtube.tf"
   "www.youtube.tf"
   "youtube.tv"
   "www.youtube.tv"
   "youtube.ws"
   "www.youtube.ws"

   ;; odysee:
   "odysee.com"
   "www.odysee.com"

   ;; twitch:
   "twitch.tv"
   "www.twitch.tv"

   ;; invidious instances:
   "yewtu.be"
   "invidious.nerdvpn.de"
   "invidious.jing.rocks"
   "inv.nadeko.net"
   "invidious.fdn.fr"
   "invidious.kavin.rocks"
   "invidious.site"
   "invidious.snopyta.org"
   "invidious.tube"
   "invidious.xyz"
   "invidious.zapashcanon.fr"
   "invidiou.site"
   "vid.mint.lgbt"

   ))


(define this-hosts-file
  (plain-file
   "hosts"

   (call-with-output-string
    (lambda (port)

      (define (display-line . words)
        (for-each
         (lambda (word i)
           (unless (= i 0) (display "	"))
           (display word port))
         words (iota (length words)))
        (newline port))

      (display-line "127.0.0.1" "localhost" this-hostname)
      (display-line "::1"       "localhost" this-hostname)

      (for-each
       (lambda (website)
         (display-line "0.0.0.0" website))
       blocked-websites)))))


(define (set-in-alist alist . key-values)
  "Removes relevant keys and sets the values back."
  (define keys (map car key-values))
  (append key-values (remove (lambda (p) (member (car p) keys)) alist)))

(define my-desktop-keyboard
  (keyboard-layout "us" #:options (list "ctrl:nocaps")))

(define my-console-keyboard
  (keyboard-layout "us" #:options (list "ctrl:nocaps")))

(define (set-display-manager services)
  (define display-manager-types (list gdm-service-type slim-service-type))
  (define my-xorg-config
    (xorg-configuration (keyboard-layout my-desktop-keyboard)))

  (cons* (service
          slim-service-type (slim-configuration
                             (xorg-configuration my-xorg-config)
                             (display ":0")
                             (vt "vt7")))
         (remove
          (lambda (service)
            (member (service-kind service) display-manager-types))
          services)))

(define (modify-special-files services)
  (modify-services
   services
   (special-files-service-type
    var =>
    (set-in-alist
     var
     ;; (list "/bin/sh" (file-append dash "/bin/dash"))
     (list "/usr/bin/env" (file-append coreutils "/bin/env"))
     ))))

(define my-libvirt-service
  (service libvirt-service-type
           (libvirt-configuration
            (unix-sock-group "libvirt"))))

(define my-virtlog-service
  (service virtlog-service-type
           (virtlog-configuration
            (max-clients 1000))))

(define (add-libvirt-services services)
  (cons* my-libvirt-service my-virtlog-service services))

(define (add-gnome-service packages)
  (cons (service gnome-desktop-service-type) packages))

(define add-my-desktop-services
  (compose add-gnome-service add-libvirt-services set-display-manager modify-special-files))

(define my-system-package-names
  '("dash"
    "glibc-locales"
    "glibc-utf8-locales-2.29"
    "localed"
    "nss-certs"
    "fontconfig"
    "font-google-noto"
    "font-google-noto-serif-cjk"
    "font-google-noto-sans-cjk"
    "font-gnu-unifont"
    "font-awesome"
    "font-microsoft-web-core-fonts"
    "font-gnu-freefont"
    "font-dejavu"
    "font-liberation"
    "font-awesome"
    "font-ghostscript"
    "font-jetbrains-mono"
    "font-wqy-microhei"
    "font-ibm-plex"
    "font-iosevka-aile"
    "font-iosevka"
    "font-inconsolata"
    "font-openmoji"
    "font-fira-code"
    "font-fira-go"
    "font-fira-sans"
    "font-fira-mono"
    ))

(define (remove-sudo-package packages)
  (define (sudo-package? package)
    (equal? "sudo" (package-name package)))

  (filter (negate sudo-package?) packages))

(define (add-my-packages packages)
  (define my-packages
    (map specification->package my-system-package-names))

  (append my-packages packages))

(define (sudo-setuid-program? prog)
  (define name
    ((compose package-name file-append-base setuid-program-program)
     prog))

  (equal? name "sudo"))

(define (remove-sudo-setuid-program progs)
  (filter (negate sudo-setuid-program?) progs))

(main)
