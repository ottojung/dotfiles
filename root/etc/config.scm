;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu))
(use-modules ((gnu packages shells) #:select (dash)))
(use-modules (nongnu packages linux)) ;; propriatary channels
(use-modules (srfi srfi-1))
(use-modules (ice-9 pretty-print))

(use-service-modules
  cups
  desktop
  networking
  virtualization
  ssh
  xorg)

(define my-desktop-keyboard
  (keyboard-layout "pl,ru" #:options (list "ctrl:swapcaps" "grp:win_space_toggle")))

(define my-console-keyboard
  (keyboard-layout "us" #:options (list "ctrl:swapcaps")))

(define my-xorg-config
  (xorg-configuration (keyboard-layout my-desktop-keyboard)))

(define (set-in-alist alist . key-values)
  "Removes relevant keys and sets the values back."
  (define keys (map car key-values))
  (append key-values (remove (lambda (p) (member (car p) keys)) alist)))






(define (set-display-manager services)
  (define display-manager-types (list gdm-service-type slim-service-type))

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
            (unix-sock-group "libvirt")
            (tls-port "16555"))))

(define (add-libvirt-service services)
  (cons my-libvirt-service services))

(define my-desktop-services
  ((compose add-libvirt-service set-display-manager modify-special-files)
   %desktop-services))

(operating-system
  ;; propriatary kernel + firmware
  (kernel linux)
  (firmware (list linux-firmware))

  (locale "en_US.UTF-8")
  (timezone "Europe/Warsaw")
  (keyboard-layout my-console-keyboard)

  (host-name "kek")
  (users (cons* (user-account
                  (name "user1")
                  (comment "user1")
                  (group "users")
                  (home-directory "/home/user1")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video" "libvirt" "lp" "tty" "input" "kvm")))
                %base-user-accounts))
  (packages
    (append
      (map specification->package '("dash" "nss-certs"))
      %base-packages))
  (services
    (cons* (service openssh-service-type)
     my-desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (targets (list "/dev/sda"))
      (keyboard-layout my-console-keyboard)))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "d7b1a4ac-b1ab-4038-9023-30a10ed50b9a"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
