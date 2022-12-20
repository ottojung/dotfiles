;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu))
(use-modules ((gnu packages shells) #:select (dash)))
(use-modules ((gnu system setuid) #:select (setuid-program-program)))
(use-modules ((guix packages) #:select (package-name)))
(use-modules (nongnu packages linux)) ;; propriatary channels
(use-modules (srfi srfi-1))

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

   (locale "en_US.UTF-8")
   (timezone "Europe/Warsaw")
   (keyboard-layout my-console-keyboard)

   (host-name "pc")

   ;; The list of user accounts ('root' is implicit).
   (users (cons* (user-account
                  (name "user1")
                  (comment "user1")
                  (group "users")
                  (home-directory "/home/user1")
                  (supplementary-groups
                   '("wheel" "netdev" "audio" "video" "libvirt" "lp" "tty" "input" "kvm")))
                 %base-user-accounts))

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

(define (set-in-alist alist . key-values)
  "Removes relevant keys and sets the values back."
  (define keys (map car key-values))
  (append key-values (remove (lambda (p) (member (car p) keys)) alist)))

(define my-desktop-keyboard
  (keyboard-layout "us" #:options (list "ctrl:swapcaps")))

(define my-console-keyboard
  (keyboard-layout "us" #:options (list "ctrl:swapcaps")))

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
