
(setq custom-file "~/.emacs.d/init-basic.el")
(load "~/.emacs.d/init-basic")
(defvar my-window-map)

(customize-set-variable 'grep-command "grep --color -HRIn")
(customize-set-variable 'grep-use-null-device nil)
(customize-set-variable 'org-ai-default-chat-model "gpt-4")
(customize-set-variable 'markdown-fontify-code-blocks-natively t)

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

(defun my-get-server-file ()
  (emacs-root-join "servers~" MY-EMACS-SERVER-NAME))

(defun my-delete-server-file ()
  (delete-file (my-get-server-file)))

(defun my-create-server-file ()
  (my-create-file
   (my-get-server-file)
   (format "%s" (emacs-pid))))

(my-create-server-file)
(add-hook 'kill-emacs-hook 'my-delete-server-file)

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
  org-ai
  :ensure nil
  :commands (org-ai-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  :config
  (setq org-ai-default-chat-model "gpt-4")
  (setq org-ai-openai-api-token my-openai-api-key)
  )

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

(when (agda-mode-exists?)
  (condition-case nil
      (load-file
       (let ((coding-system-for-read 'utf-8))
         (shell-command-to-string "agda-mode locate")))
    (error (message "Could not load agda-mode"))))

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


;;;;;;;;;;;;;;;;;;;
;; MY AI HELPERS ;;
;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

(require 'org-element)

(defun my-inside-ai-org-block-p ()
  (let ((element (org-element-context)))
    (while (and element
                (not (and (eq (car element) 'special-block)
                          (string= (plist-get (nth 1 element) :type) "ai"))))
      (setq element (plist-get (nth 1 element) :parent)))

    (and element
         (<= (point)
             (plist-get (nth 1 element) :contents-end))
         (>= (point)
             (plist-get (nth 1 element) :contents-begin))
         element)))

(defun my-chatfile-insert-block (&optional content)
  "Insert AI block with `content' and place cursor ready for input."
  (interactive)

  ;; Check if we're in a #+begin_ai block
  (when-let ((ai-block (my-inside-ai-org-block-p)))
    (goto-char (plist-get (nth 1 ai-block) :contents-end)))

  (end-of-line)
  (newline)
  (insert
   (format "
#+begin_ai markdown
%s
#+end_ai" (or content "[ME]: ")))
  (end-of-line)
  (newline)

  ;; Move back three lines to position cursor right after [ME]:
  (forward-line -2)
  (end-of-line)
  )

(defun my-chatfile-create-generic (get-file-name &optional content)
  (let* ((dir (or (when-let ((session (getenv "MY_SESSION_DIRECTORY")))
                    (my-path-join session "wd"))
                  (when-let ((root (getenv "MY_ROOT")))
                    (my-path-join root "tmp"))
                  temporary-file-directory))

         (dir-full (my-path-join dir "chat")))

    (unless (file-exists-p dir-full)
      (make-directory dir-full t))

    (let* ((file-name (funcall get-file-name (directory-files dir-full nil "chat-.*")))

           (path (my-path-join dir-full file-name))

           )

      (let ((buf (find-file path)))
        (with-current-buffer buf
          (my-chatfile-insert-block content))))))

(defun my-chatfile-new ()
  "Create a file in a specified or default directory and open it."
  (interactive)
  (let* ((full-name (buffer-file-name (current-buffer)))
         (current-name (and full-name (file-name-nondirectory full-name))))
    (if (and full-name (my-extract-numbers-from-filename current-name))
        (my-chatfile-create-generic
         (lambda (existing)
           (my-chatname-increment current-name existing)))
        (my-chatfile-create-generic #'my-chatname-restart))))

(defun my-get-text-from-point-to-cursor (start-point)
  "Get text from START-POINT to current cursor position"
  (interactive "nStart Point: ")
  (let ((end-point (point)))
    (buffer-substring-no-properties start-point end-point)))

(defun my-chatfile-fork ()
  "Create a file in a specified or default directory and open it."
  (interactive)

  (let* ((ai-block (my-inside-ai-org-block-p))
         (start (if ai-block (plist-get (nth 1 ai-block) :contents-begin)
                  (point)))
         (text0 (my-get-text-from-point-to-cursor start))
         (text (if (string-empty-p text0) nil text0)))

    (my-chatfile-create-generic
     (lambda (existing)
       (let ((full-name (buffer-file-name (current-buffer))))
         (unless full-name
           (error "This function must be called from a buffer associated with some file."))

         (let ((current-name (file-name-nondirectory full-name)))
           (unless (my-extract-numbers-from-filename current-name)
             (error "This function must be called from a file called \"chat-something.org\"."))

           (my-chatname-fork current-name existing))))

     text)))


(defun my-org-mode-keys-hook ()
  (define-key org-mode-map
    (kbd "M-o a b")
    'my-chatfile-insert-block))

(add-hook 'org-mode-hook 'my-org-mode-keys-hook)

(defun my-extract-numbers-from-filename (filename)
  "Extract numbers from the filename in the form of `chat-001.org`, `chat-002-003.org`"

  ;; ; Use it like this:
  ;; (extract-numbers-from-filename "chat-001.org") ; => (1)
  ;; (extract-numbers-from-filename "chat-002-003.org") ; => (2 3)
  ;; (extract-numbers-from-filename "chat-007.org") ; => (7)
  ;; (extract-numbers-from-filename "chat-008-02-9374.org") ; => (8 2 9374)
  ;; (extract-numbers-from-filename "chat.org") ; => nil
  ;; (extract-numbers-from-filename "008-02-9374.org") ; => nil

  (when (string-match "chat-.+[.]org" filename)
    (let ((start 0)
          numbers)
      (while (string-match "\\([0-9]+\\)" filename start)
        (setq start (match-end 0))
        (push (string-to-number (match-string 1 filename)) numbers))
      (nreverse numbers))))

(defun my-chatname-parse-existing (existing)
  (let* ((num-lists (mapcar #'my-extract-numbers-from-filename existing))
         (nonnuls (delete nil num-lists))
         (nums (mapcar #'car nonnuls)))
    nums))

(defun my-chatname-restart (existing)
  ;; Example:
  ;;
  ;; (my-chatname-restart
  ;;  '("chat-001.org"
  ;;    "chat.org"
  ;;    "whatever"
  ;;    "chat-007.org"
  ;;    "chat-015-002.org"
  ;;    "chat-008.org"
  ;;    ))
  ;;   =>
  ;; "chat-016.org"
  ;;
  ;; (my-chatname-restart '())
  ;;   =>
  ;; "chat-001.org

  (let* ((nums (my-chatname-parse-existing existing))
         (sorted (sort nums #'>))
         (highest (if sorted (car sorted) 0)))

    (format "chat-%03d.org" (+ 1 highest))))

(defun my-is-prefix (lst1 lst2)
  "Check if lst1 is a prefix of lst2."
  (cond ((null lst1) t)
        ((null lst2) nil)
        ((not (equal (car lst1) (car lst2))) nil)
        (t (my-is-prefix (cdr lst1) (cdr lst2)))))

(defun my-increment-last-number (num-list)
  (if (null num-list)
      nil
    (let ((last-num (car (last num-list))))
      (setcar (last num-list) (+ 1 last-num))
      num-list)))

(defun my-versions-increment (existing-nums current-nums)
  (let* ((this-len (length current-nums))
         (samelength-nums
          (mapcar
           (lambda (item) (my-list-take this-len item))
           (cons current-nums existing-nums)))
         (sorted (my-sort-numeric-lists/descending samelength-nums))
         (highest (car sorted))
         (new-nums (my-increment-last-number highest))
         )

    new-nums))

(defun my-chatname-increment (current existing)

  ;; (my-chatname-increment
  ;;  "chat-015.org"
  ;;  '("chat-001.org"
  ;;    "chat.org"
  ;;    "whatever"
  ;;    "chat-007.org"
  ;;    "chat-015-002.org"
  ;;    "chat-008.org"))
  ;; =>
  ;; "chat-016.org"

  ;; (my-chatname-increment
  ;;  "chat-015.org"
  ;;  '("chat-001.org"
  ;;    "chat.org"
  ;;    "whatever"
  ;;    "chat-007.org"
  ;;    "chat-015.org"
  ;;    "chat-008.org"))
  ;; =>
  ;; "chat-016.org"

  ;; (my-chatname-increment
  ;;  "chat-015.org"
  ;;  '("chat-001.org"
  ;;    "chat.org"
  ;;    "whatever"
  ;;    "chat-007.org"
  ;;    "chat-015-000.org"
  ;;    "chat-008.org"))
  ;; =>
  ;; "chat-016.org"

  ;; (my-chatname-increment
  ;;  "chat-015.org"
  ;;  '("chat-001.org"
  ;;    "chat.org"
  ;;    "whatever"
  ;;    "chat-007.org"
  ;;    "chat-015-000-13412.org"
  ;;    "chat-008.org"))
  ;; =>
  ;; "chat-016.org"

  ;; (my-chatname-increment
  ;;  "chat-015-001.org"
  ;;  '("chat-001.org"
  ;;    "chat.org"
  ;;    "whatever"
  ;;    "chat-007.org"
  ;;    "chat-015-002.org"
  ;;    "chat-008.org"))
  ;; =>
  ;; "chat-015-003.org"

  ;; (my-chatname-increment
  ;;  "chat-015-001.org"
  ;;  '("chat-001.org"
  ;;    "chat.org"
  ;;    "whatever"
  ;;    "chat-007.org"
  ;;    "chat-015.org"
  ;;    "chat-008.org"))
  ;; =>
  ;; "chat-015-002.org"

  ;; (my-chatname-increment
  ;;  "chat-015-001.org"
  ;;  '("chat-001.org"
  ;;    "chat.org"
  ;;    "whatever"
  ;;    "chat-007.org"
  ;;    "chat-015-000.org"
  ;;    "chat-008.org"))
  ;; =>
  ;; "chat-015-002.org"

  ;; (my-chatname-increment
  ;;  "chat-015-001.org"
  ;;  '("chat-001.org"
  ;;    "chat.org"
  ;;    "whatever"
  ;;    "chat-007.org"
  ;;    "chat-015-000-13412.org"
  ;;    "chat-008.org"))
  ;; =>
  ;; "chat-015-002.org"

  (let* ((existing-nums (delete nil (mapcar #'my-extract-numbers-from-filename existing)))
         (current-nums (my-extract-numbers-from-filename current))
         (new-nums (my-versions-increment existing-nums current-nums))
         )

    (format "chat-%s.org"
            (string-join
             (mapcar (lambda (x) (format "%03d" x))
                     new-nums)
             "-"))))

(defun my-list-drop-prefix (prefix list)
  "Drop PREFIX from LIST."
  (if (equal (length prefix) 0)
      list
    (if (equal (car prefix) (car list))
        (my-list-drop-prefix (cdr prefix) (cdr list))
      (error "Prefix list is not a prefix of the second one"))))

(defun my-versions-fork (existing-nums current-nums)
  (let* ((sameprefix-nums
          (remove-if-not
           (lambda (item)
             (and (my-is-prefix current-nums item)
                  (not (equalp current-nums item))))
           existing-nums))

         (without-prefix-nums
          (delete
           nil
           (mapcar
            (lambda (x)
              (my-list-drop-prefix current-nums x))
            sameprefix-nums)))

         (max-existing-nums
          (car
           (if without-prefix-nums
               (car
                (sort without-prefix-nums
                      (lambda (a b)
                        (> (car a) (car b)))))

             (list 0))))

         )

    (append current-nums (list (+ 1 max-existing-nums)))))

(defun my-chatname-fork (current existing)

  ;; Examples:
  ;;
  ;; (my-chatname-fork
  ;;  "chat-015.org"
  ;;  '("chat-001.org"
  ;;    "chat.org"
  ;;    "whatever"
  ;;    "chat-007.org"
  ;;    "chat-015-002.org"
  ;;    "chat-008.org"))
  ;;    =>
  ;; "chat-015-003.org"
  ;;
  ;; (my-chatname-fork
  ;;  "chat-007.org"
  ;;  '("chat-001.org"
  ;;    "chat.org"
  ;;    "whatever"
  ;;    "chat-007.org"
  ;;    "chat-015-002.org"
  ;;    "chat-008.org"))
  ;;    =>
  ;; "chat-007-001.org"

  (let* ((existing-nums (delete nil (mapcar #'my-extract-numbers-from-filename existing)))
         (current-nums (my-extract-numbers-from-filename current))
         (new-nums (my-versions-fork existing-nums current-nums))
         )

    (format "chat-%s.org"
            (string-join
             (mapcar (lambda (x) (format "%03d" x))
                     new-nums)
             "-"))))

;;;;;;;;;;;;;
;; THE END ;;
;;;;;;;;;;;;;
