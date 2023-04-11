
(setq custom-file "~/.emacs.d/init-basic.el")
(load "~/.emacs.d/init-basic")
(defvar my-window-map)

(customize-set-variable 'grep-command "grep --color -HRIn")
(customize-set-variable 'grep-use-null-device nil)

(let ((private (emacs-root-join "private.el")))
  (when (file-exists-p private)
    (load private)))

;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(eval-when-compile
  (add-to-list 'load-path (emacs-root-join "custom" "use-package"))
  (require 'use-package))
(require 'subr-x) ;; for "string-trim" and other string operations
(require 'face-remap) ;; for custom face mode
(require 'bind-key)

;; reopen last session buffers
;; (desktop-save-mode 1)

;; ;; save buffer on emacs lost focus (not buffer lost focus)
;; (defun save-buffer-on-lost ()
;;         (interactive)
;;         (if (or (null buffer-file-name) (not (file-writable-p buffer-file-name)))
;;             () ;; not writable
;;             (save-buffer)))
;; (add-hook 'focus-out-hook 'save-buffer-on-lost)

;; current emacs session env variable

(defconst MY-EMACS-SERVER-NAME
  (or (getenv "EMACS_SERVER_NAME")
      (concat "emacs-server-" (number-to-string (random)))))

(setenv "EMACS_SERVER_NAME" MY-EMACS-SERVER-NAME)

(defvar server-name)
(setq server-name MY-EMACS-SERVER-NAME)

(defvar MY-EMACS-SERVER-STARTED nil)

(unless MY-EMACS-SERVER-STARTED
  (setq MY-EMACS-SERVER-STARTED t)
  (server-start))

;;;;;;;;;;
;; MAIL ;;
;;;;;;;;;;

;; defined in sendmail package
(defvar send-mail-function) ;; SUPRESS WARNING
(defvar sendmail-program) ;; SUPRESS WARNING
(defvar mail-specify-envelope-from) ;; SUPRESS WARNING
(defvar message-sendmail-envelope-from) ;; SUPRESS WARNING
(defvar mail-envelope-from) ;; SUPRESS WARNING

; Outgoing email (msmtp + msmtpq)
(setq send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(defun my-message-setup-hook ()
  (gnus-alias-determine-identity)
  ;; (define-key message-mode-map (kbd "C-c f")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (message-remove-header "Fcc")
  ;;     (message-remove-header "Organization")
  ;;     ;; (gnus-alias-select-identity)
  ;;     (notmuch-fcc-header-setup)))
  (flyspell-mode))

(add-hook 'message-setup-hook 'my-message-setup-hook)



(defvar gnus-button-url) ;; SUPRESS WARNING
(defvar browse-url-generic-program) ;; SUPRESS WARNING
(defvar browse-url-browser-function) ;; SUPRESS WARNING

(setq gnus-button-url 'browse-url-generic
      browse-url-generic-program "my-browser-open-link"
      browse-url-browser-function gnus-button-url)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; RUN LAST IN TERM ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my-is-mt-term-window-regexp
  (concat
   (regexp-quote "*my-term-mt")
   "[[:alnum:]]+"
   (regexp-quote "*")))
(defun my-is-mt-term-window (win)
  (string-match-p
   my-is-mt-term-window-regexp
   (buffer-name (window-buffer win))))

(defun my-run-last-term-command ()
  (interactive)
  (let ((current-w (selected-window))
        (term-w
         (get-window-with-predicate #'my-is-mt-term-window)))
    (if term-w
        (progn
          (my-save-current-buffer-sync)
          (select-window term-w)
          (execute-kbd-macro (kbd "<up>"))
          (execute-kbd-macro (kbd "RET"))
          (select-window current-w))
      (error (format "Please open a terminal first")))))

;;;;;;;;;;;;;;;;
;; LOOK/THEME ;;
;;;;;;;;;;;;;;;;

;; (customize-set-variable 'custom-enabled-themes '(abyss))
(eval-when-compile
  (add-to-list 'load-path (emacs-root-join "custom" "modus-themes")))

(add-to-list 'load-path (emacs-root-join "custom" "modus-themes"))
(require 'modus-themes)
(setq modus-themes-diffs 'deuteranopia)
(modus-themes-load-vivendi)

;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;

(defun my-haskell-mode-hook ()
  (haskell-tab-indent-mode))
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

;; R-mode: no fancy comments
(customize-set-variable 'ess-indent-with-fancy-comments nil)

(use-package flycheck
  :after web-mode
  :diminish flycheck-mode)


(let ((translate-path (emacs-root-join "custom" "translate.el" "translate.el")))
  (when (file-exists-p translate-path)
    (load translate-path)
    (setq translate-languages '("English"))
    (defun my-translate ()
      (interactive)
      (let ((current-prefix-arg '(4)))
        (call-interactively 'translate)))))

(defun my-prolog-consult-file-advice (orig-fun)
  "Saves file before evaluating it."
  (my-save-buffer-force)
  (funcall orig-fun))

(advice-add 'prolog-consult-file :around #'my-prolog-consult-file-advice)

;;;;;;;;;;;;;;;;
;; Typescript ;;
;;;;;;;;;;;;;;;;

(customize-set-variable
 'flycheck-check-syntax-automatically
 '(save mode-enabled))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(customize-set-variable 'company-tooltip-align-annotations t)

;; TSX
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :hook (
         (typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ;; (before-save . tide-format-before-save)
         )
  )

;;;;;;;;;;;;;;;;;;;;
;; Other packages ;;
;;;;;;;;;;;;;;;;;;;;

;; disable annoying lua-indent
(defun lua-indent-line (&rest args) nil)

(use-package
 buffer-flip
 :ensure nil
 :config

 (define-key my-window-map (kbd "M-p") 'buffer-flip)
 (define-key my-window-map (kbd "M-n")
   #'(lambda ()
       (buffer-flip)
       (buffer-flip-forward)
       (buffer-flip-forward)
       (buffer-flip-forward)))

 (defvar buffer-flip-map)
 ;; transient keymap used once cycling starts
 (setq buffer-flip-map
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "M-p") 'buffer-flip-forward)
         (define-key map (kbd "M-n") 'buffer-flip-backward)
         (define-key map (kbd "M-g") 'buffer-flip-abort)
         map))

 )

(defun my-elfeed-update ()
  (interactive)
  (elfeed-update)
  (elfeed-search--update-list))

(defvar elfeed-search-mode-map)

(use-package elfeed
  :bind (:map elfeed-search-mode-map
              ("g" . 'my-elfeed-update))

  :config
  ;; Save elfeed db automatically, because if Emacs crashes or is killed (which happens to me
  ;; occasionally, especially since I develop packages in a single instance), we'd lose the db
  ;; updates not saved.
  (unless (cl-loop for timer in timer-idle-list
                   thereis (equal (aref timer 5) #'elfeed-db-save))
    (run-with-idle-timer 10000 'repeat #'elfeed-db-save)))

; @begin(56867949)@ - Do not edit these lines - added automatically!
(defconst my-ciaoroot (user-home-join ".ciaoroot" "master" "ciao_emacs" "elisp"))
(if (file-exists-p (concat my-ciaoroot "/ciao-site-file.el"))
  (load-file (concat my-ciaoroot "/ciao-site-file.el")))
; @end(56867949)@ - End of automatically added lines.

;; don't skip same buffer
(defun my-buffer-flip-skip-buffer-advice
    (orig-fun buf)
  (or (= ? (elt (buffer-name buf) 0)) ; internal?
      (eq (get-buffer-window buf)
          (get-buffer-window (current-buffer)))))
(advice-add 'buffer-flip-skip-buffer
            :around #'my-buffer-flip-skip-buffer-advice)

(defun agda-mode-exists? ()
  (= 0 (car (list (call-process "which" nil nil nil "agda-mode")))))

(if (agda-mode-exists?)
    (condition-case nil
        (load-file
         (let ((coding-system-for-read 'utf-8))
           (shell-command-to-string "agda-mode locate")))
      (error (message "Could not load agda-mode")))
    (message "%s" "Skipping `agda-mode' as it is not installed"))

(defvar company-active-map)
(with-eval-after-load 'company
  (let ((m company-active-map))
    (define-key m [return] nil)
    (define-key m (kbd "RET") nil)
    (define-key m [tab] 'company-complete-selection)
    (define-key m (kbd "TAB") 'company-complete-selection)
    (define-key m (kbd "C-n") 'company-select-next)
    (define-key m (kbd "C-p") 'company-select-previous)

    ;; ;; optionaly don't use same keys as for movement
    ;; (define-key m (kbd "C-n") nil)
    ;; (define-key m (kbd "C-p") nil) ; optionaly don't use same keys as for movement
    ;; (define-key m [backtab] 'company-select-next)
    ;; (define-key m (kbd "S-TAB") 'company-select-next)
    )
  )

(use-package company
             :ensure nil
             :config
             (setq company-minimum-prefix-length 1)
             (setq company-idle-delay 0))

(add-hook 'after-init-hook 'global-company-mode)

(add-hook
 'coq-mode-hook
 (lambda ()
   (defvar coq-mode-map) ;; SUPRESS WARNING
   (define-key coq-mode-map (kbd "M-a") nil)
   (define-key coq-mode-map (kbd "M-e") nil)))

(use-package company-tabnine
  :ensure nil)

(defvar company-tabnine-restart-enabled nil)

(defun company-tabnine-toggle ()
  (interactive)
  (setq company-tabnine-restart-enabled (not company-tabnine-restart-enabled))
  (company-tabnine-kill-process)
  (company-tabnine-restart-server)
  (if company-tabnine-restart-enabled
      (progn
        (add-to-list 'company-backends #'company-tabnine)
        (message "tabnine was turned on"))
    (progn
      (setq company-backends
            (delete #'company-tabnine company-backends))
      (message "tabnine was turned off"))))

(defun company-tabnine-start-process-wrapper (orig-fun &rest args)
  (if company-tabnine-restart-enabled
      (apply orig-fun args)))

(advice-add 'company-tabnine-start-process :around #'company-tabnine-start-process-wrapper)

(haskell-tab-indent-mode 1)

(use-package projectile
  :ensure nil
  :init (projectile-mode))

;;;;;;;;;;;;;;;;;;;;;
;; CYRILLIC LAYOUT ;;
;;;;;;;;;;;;;;;;;;;;;

(defun translate-keystrokes-ru->en ()
  "Make emacs output english characters, regardless whether
the OS keyboard is english or russian"
  (let ((make-key-stroke (lambda (prefix char)
                           (eval `(kbd ,(if (and (string-match "^C-" prefix)
                                                 (string-match "[A-Z]" (string char)))
                                            (concat "S-" prefix (string (downcase char)))
                                          (concat prefix (string char)))))))
        (case-fold-search nil)
        (keys-pairs (zip-with 'cons
                              "йцукенгшщзхъїфіывапролджєэґячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
                              "qwertyuiop[]]assdfghjkl;''\\zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"))
        ;; (prefixes '("" "s-" "M-" "M-s-"
        ;; "C-" "C-s-" "C-M-" "C-M-s-")))
        (prefixes '( "s-" "M-" "M-s-"
                     "C-" "C-s-" "C-M-" "C-M-s-")))
    (mapc (lambda (prefix)
            (mapc (lambda (pair)
                    (define-key key-translation-map
                      (funcall make-key-stroke prefix (car pair))
                      (funcall make-key-stroke prefix (cdr pair))))
                  keys-pairs))
          prefixes)))

;;   ;; Uncomment below if want to disable all cyrillic and use latin instead
;;   (defun literal-insert ()
;;     (interactive)
;;     (insert-char last-input-event 1))

;;   (define-minor-mode literal-insert-mode
;;     "Make emacs output characters corresponging to the OS keyboard,
;; ignoring the key-translation-map"
;;     :keymap (let ((new-map (make-sparse-keymap))
;;                   (english-chars "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"))
;;               (mapc (lambda (char)
;;                       (define-key new-map (string char)
;;                         'literal-insert))
;;                     english-chars)
;;               new-map))

;;   (defun run-literal-mode ()
;;     (literal-insert-mode 1))

;;   (add-hook 'text-mode-hook 'run-literal-mode)

(defun support-cyrillic-layout ()
  "Make emacs output english characters, regardless whether
the OS keyboard is english or russian"

  (translate-keystrokes-ru->en))

(support-cyrillic-layout)

;;;;;;;;;;;;;
;; THE END ;;
;;;;;;;;;;;;;
