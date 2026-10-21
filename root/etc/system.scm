;; Minimal headless Guix System configuration for billy.
(use-modules (gnu))
(use-modules ((gnu system setuid) #:select (setuid-program-program))
             ((guix packages) #:select (package-name))
             (nongnu packages linux)
             (srfi srfi-1))
(use-modules (gnu services shepherd))
(use-service-modules desktop docker networking ssh)
(use-package-modules docker)

(define my-console-keyboard
  (keyboard-layout "us" #:options (list "ctrl:nocaps")))

(define (remove-sudo-package packages)
  (filter (lambda (package)
            (not (equal? "sudo" (package-name package))))
          packages))

(define (remove-sudo-setuid-program progs)
  (filter (lambda (prog)
            (not (equal? "sudo"
                         (package-name
                          (file-append-base (setuid-program-program prog))))))
          progs))

;; The Lubko container explicitly bind-mounts the host's /usr/bin/env.
(define (keep-usr-bin-env services)
  (modify-services services
    (special-files-service-type
     files =>
     (cons (list "/usr/bin/env" (file-append coreutils "/bin/env"))
           (remove (lambda (entry)
                     (equal? "/usr/bin/env" (car entry)))
                   files)))))

(define (billy-startup-services)
  (list
   (shepherd-service
    (documentation "Keep billy's reverse SSH tunnel connected.")
    (provision '(billy-ssh-tunnel))
    (requirement '(user-processes networking))
    (start #~(make-forkexec-constructor
              '("/bin/sh" "-c" ". \"$HOME/.profile\"; exec my-make-ssh-tunnel")
              #:user "user1"
              #:group "user1g"
              #:directory "/home/user1"
              #:file-creation-mask #o077
              #:log-file "/home/user1/.local/state/billy-ssh-tunnel.log"
              #:environment-variables
              '("HOME=/home/user1" "PATH=/run/current-system/profile/bin:/bin")))
    (stop #~(make-kill-destructor))
    (respawn? #t))

   (shepherd-service
    (documentation "Run billy-dev's Lubko Docker container.")
    (provision '(billy-lubko-container))
    (requirement '(user-processes networking dockerd))
    (start #~(make-forkexec-constructor
              '("/bin/sh" "-c" ". \"$HOME/.profile\"; exec my-lubko-container-start")
              #:user "user1"
              #:group "user1g"
              #:supplementary-groups '("docker")
              #:directory "/home/user1"
              #:file-creation-mask #o077
              #:log-file "/home/user1/.local/state/billy-lubko-container.log"
              #:environment-variables '("HOME=/home/user1" "PATH=/run/current-system/profile/bin:/bin")))
    (stop #~(make-kill-destructor))
    (respawn? #t))))

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (timezone "US/Pacific")
 (keyboard-layout my-console-keyboard)
 (host-name "billy")

 ;; Preserve both local users, with no desktop-related privileges.
 (users (cons*
         (user-account
          (name "user1")
          (comment "user1")
          (group "user1g")
          (home-directory "/home/user1")
          (supplementary-groups '("wheel" "netdev" "docker")))
         (user-account
          (name "user2")
          (comment "user2")
          (group "user2g")
          (home-directory "/home/user2"))
         %base-user-accounts))
 ;; docker-service-type provisions the docker group.
 (groups (cons* (user-group (name "user1g"))
                (user-group (name "user2g"))
                %base-groups))

 (packages (cons docker-cli (remove-sudo-package %base-packages)))
 (setuid-programs
  (remove-sudo-setuid-program %setuid-programs))

 (services
  (keep-usr-bin-env
   (cons*
    (service openssh-service-type)
    (service wpa-supplicant-service-type)
    (service network-manager-service-type)
    (service containerd-service-type)
    (service elogind-service-type)
    (service docker-service-type)
    (simple-service 'billy-startup
                    shepherd-root-service-type
                    (billy-startup-services))
    %base-services)))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets (list "/boot/efi"))
   (keyboard-layout keyboard-layout)))

 (mapped-devices
  (list (mapped-device
         (source (uuid "8b37a5aa-dd43-4f9c-901a-8e488ae6e701"))
         (target "cryptroot")
         (type luks-device-mapping))))

 (file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device (uuid "4ED6-B9A2" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device "/dev/mapper/cryptroot")
          (type "ext4")
          (dependencies mapped-devices))
         %base-file-systems)))
