
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#454545" "#cd5542" "#6aaf50" "#baba36" "#5180b3" "#ab75c3" "#68a5e9" "#bdbdb3"])
 '(backward-delete-char-untabify-method 'hungry)
 '(blink-cursor-mode nil)
 '(compilation-disable-input t)
 '(compilation-scroll-output 'first-error)
 '(confirm-nonexistent-file-or-buffer nil)
 '(custom-safe-themes
   '("ddff22007104a1317014e48ff3d4911a83771a4ccf57185ccebf7f91339dbfb8" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" default))
 '(default-input-method "TeX")
 '(dired-dwim-target t)
 '(dired-hide-details-hide-information-lines t)
 '(dired-hide-details-hide-symlink-targets nil)
 '(dired-listing-switches "-alFhgG")
 '(inhibit-startup-screen t)
 '(mail-host-address "tickle-tickle")
 '(menu-bar-mode nil)
 '(message-default-mail-headers "Cc:
Bcc:
")
 '(org-agenda-default-appointment-duration 90 t)
 '(org-startup-truncated nil)
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(esxml consult plz magit projectile maxima yaml-mode web-mode tide typescript-mode proof-general csharp-mode ess nix-mode json-mode elfeed gnus-alias rust-mode lua-mode buffer-flip notmuch lean-mode ample-theme abyss-theme live-py-mode racket-mode popup magit-todos idris-mode company-tabnine company-ghc))
 '(scroll-bar-mode nil)
 '(send-mail-function 'smtpmail-send-it)
 '(shell-file-name "/bin/sh")
 '(smtpmail-smtp-server "smtp.office365.com")
 '(smtpmail-smtp-service 25)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(vc-follow-symlinks t)
 '(whitespace-style '(face trailing tab-mark)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;
;; BASIC ;;
;;;;;;;;;;;

;; dont ask for comfirmation when killing a buffer with a process in it
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

(setq delete-by-moving-to-trash t)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;
;;; set up unicode
(unless (getenv "LC_ALL")
  (setenv "LC_ALL" "en_US.UTF-8")
  (setenv "LANG" "en_US.UTF-8"))
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(defvar default-buffer-file-coding-system)
(setq default-buffer-file-coding-system 'utf-8)
(defvar x-select-request-type)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(defun my-path-join* (rest)
  (if (cdr rest)
      (concat (file-name-as-directory (car rest)) (my-path-join* (cdr rest)))
    (car rest)))
(defun my-path-join (&rest args)
  (my-path-join* args))

(defconst emacs-root-init-dir (file-name-directory load-file-name))
(defun emacs-root-join (&rest args)
  (apply 'my-path-join (cons emacs-root-init-dir args)))

(defconst user-home-dir (getenv "HOME"))
(defun user-home-join (&rest args)
  (apply 'my-path-join (cons user-home-dir args)))


(defvar tramp-backup-directory-alist) ;; SUPRESS WARNING
(defvar tramp-auto-save-directory) ;; SUPRESS WARNING

;; Tmp dir for saving backups
;; Put backup files neatly away
(let ((backup-dir (emacs-root-join "backups~"))
      (auto-saves-dir (emacs-root-join "autosaves~")))

  (dolist (dir (list backup-dir auto-saves-dir))
    (when (file-symlink-p dir)
      (delete-file dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (my-path-join auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

;; (setq backup-by-copying t    ; Don't delink hardlinks
;;       ;; delete-old-versions t  ; Clean up the backups
;;       version-control t      ; Use version numbers on backups,
;;       kept-new-versions 5    ; keep some new versions
;;       kept-old-versions 2)   ; and some old ones, too

;; align with spaces intead of tabs
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;; Don't disable downcase
(put 'downcase-region 'disabled nil)

(defun ask-user-about-supersession-threat (fn)
  "blatantly ignore files that changed on disk"
  )
(defun ask-user-about-lock (file opponent)
  "always grab lock"
  t)

;;;;;;;;;;;;;
;; HELPERS ;;
;;;;;;;;;;;;;

(defsubst string-empty-p (string)
  (string= string ""))

(defsubst string-trim-left (string &optional regexp)
  (if (string-match (concat "\\`\\(?:" (or regexp "[ \t\n\r]+") "\\)") string)
      (substring string (match-end 0))
    string))

(defsubst string-trim-right (string &optional regexp)
  (let ((i (string-match-p (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'")
                           string)))
    (if i (substring string 0 i) string)))

(defsubst string-trim (string &optional trim-left trim-right)
  (string-trim-left (string-trim-right string trim-right) trim-left))

(defun sh (proc &rest args)
  (string-trim-right
   (car (apply 'call-process-with-output (cons proc args)))))

(defun set-in-alist (alist key value)
  (let ((cur alist)
        (ret nil)
        (repl (cons key value))
        (foundq nil))
    (while cur
      (let* ((cell (car cur))
             (iseq (equal key (car cell)))
             (new (if iseq repl cell)))
        (if iseq (setq foundq t))
        (setq ret (cons new ret)))
      (setq cur (cdr cur)))

    (unless foundq
      (setq ret (cons repl ret)))

    ret))

(defmacro set-in-alist! (alist key value)
  `(setq ,alist (set-in-alist ,alist ,key ,value)))

(defun cl--mapcar-many (cl-func cl-seqs &optional acc)
  (if (cdr (cdr cl-seqs))
      (let* ((cl-res nil)
             (cl-n (apply 'min (mapcar 'length cl-seqs)))
             (cl-i 0)
             (cl-args (copy-sequence cl-seqs))
             cl-p1 cl-p2)
        (setq cl-seqs (copy-sequence cl-seqs))
        (while (< cl-i cl-n)
          (setq cl-p1 cl-seqs cl-p2 cl-args)
          (while cl-p1
            (setcar cl-p2
                    (if (consp (car cl-p1))
                        (prog1 (car (car cl-p1))
                          (setcar cl-p1 (cdr (car cl-p1))))
                      (aref (car cl-p1) cl-i)))
            (setq cl-p1 (cdr cl-p1) cl-p2 (cdr cl-p2)))
          (if acc
              (push (apply cl-func cl-args) cl-res)
            (apply cl-func cl-args))
          (setq cl-i (1+ cl-i)))
        (and acc (nreverse cl-res)))
    (let ((cl-res nil)
          (cl-x (car cl-seqs))
          (cl-y (nth 1 cl-seqs)))
      (let ((cl-n (min (length cl-x) (length cl-y)))
            (cl-i -1))
        (while (< (setq cl-i (1+ cl-i)) cl-n)
          (let ((val (funcall cl-func
                              (if (consp cl-x) (pop cl-x) (aref cl-x cl-i))
                              (if (consp cl-y) (pop cl-y) (aref cl-y cl-i)))))
            (when acc
              (push val cl-res)))))
      (and acc (nreverse cl-res)))))

(defun zip-with (cl-func cl-x &rest cl-rest)
  "Apply FUNCTION to each element of SEQ, and make a list of the results.
If there are several SEQs, FUNCTION is called with that many arguments,
and mapping stops as soon as the shortest list runs out.  With just one
SEQ, this is like `mapcar'.  With several, it is like the Common Lisp
`mapcar' function extended to arbitrary sequence types.
\n(fn FUNCTION SEQ...)"
  (if cl-rest
      (if (or (cdr cl-rest) (nlistp cl-x) (nlistp (car cl-rest)))
          (cl--mapcar-many cl-func (cons cl-x cl-rest) 'accumulate)
        (let ((cl-res nil) (cl-y (car cl-rest)))
          (while (and cl-x cl-y)
            (push (funcall cl-func (pop cl-x) (pop cl-y)) cl-res))
          (nreverse cl-res)))
    (mapcar cl-func cl-x)))

(provide 'zip-with)

(defun repeat-n (n f)
    (funcall f)
    (if (> n 0) (repeat-n (- n 1) f)))

(provide 'repeat-n)

(defun list-to-string (list)
  (mapconcat 'string list ""))

(provide 'list-to-string)

;;;; scrolling

(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 4)))

(defun scroll-up-half ()
  (interactive)
  (scroll-up (window-half-height))
  )

(defun scroll-down-half ()
  (interactive)
  (scroll-down (window-half-height))
  )

;;;; buffers

(defun my-save-buffer-force ()
  (let ((save-silently t)
        (inhibit-message t))
    (basic-save-buffer-1)))

(defun my-save-buffer (buf)
  (when (and (buffer-file-name buf)
             (buffer-modified-p buf))
    (with-current-buffer buf
      (make-thread 'my-save-buffer-force))))

(defun my-save-current-buffer ()
  (my-save-buffer (current-buffer)))

(defun my-save-current-buffer-sync ()
  (let ((buf (current-buffer)))
    (when (and (buffer-file-name buf)
               (buffer-modified-p buf))
      (my-save-buffer-force))))

(defun kill-buffer-fast ()
  "Saves and kills CURRENT buffer."
  (interactive)
  (when (and buffer-file-name
             (file-writable-p buffer-file-name))
    (my-save-current-buffer-sync))
  (kill-buffer (current-buffer)))

;;;; editor
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode)
  )

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode)
  )

;;;; other

(defun my-grep ()
  (interactive)
  (let ((arg (read-shell-command "grep: " nil 'grep-history)))
    (let ((command (concat grep-command " " arg)))
      (compilation-start
       command
       'grep-mode))))

(defun program-resolve-path (program)
  (string-trim
   (car
    (call-process-with-output "which" program))))

(defun call-process-with-output (proc &rest args)
  (let ((return-code -1))
    (list
     (with-output-to-string
       (with-current-buffer
           standard-output
         (setq return-code (apply 'call-process (append (list proc nil t nil) args)))))
     return-code)))

(defun my-git-save ()
  (interactive)
  (my-save-current-buffer-sync)
  (let ((out (call-process-with-output "/bin/sh" "-c" "git add --update && git status --short && git commit --message 'save'")))
    (message "%s" (string-trim (car out)))))

(defun my-git-status ()
  (interactive)
  (my-save-current-buffer-sync)
  (let ((out (sh "git" "status" "--short")))
    (when (string-empty-p out)
      (setq out (sh "git" "status")))
    (message "%s" out)))

(defun my-revert-buffer ()
  (interactive)
  (message
   (if (revert-buffer t t t)
       "I reverted buffer :)"
     "I failed you, sensei")))

(defun my-copy-filename ()
  (interactive)
  (let ((name (buffer-file-name)))
    (if name
        (progn
          (kill-new name)
          (message (format "Copied %s" name))
          name)
      (message (format "Buffer %s is not associated with any file" (buffer-name))))))

(defun my-fix-imports ()
  "Tries to fix missing imports in the current file."
  (interactive)
  (my-save-buffer-force)
  (let ((filepath (buffer-file-name (current-buffer))))
    (let ((out (sh "my-fix-imports" filepath)))
      (message "%s" out)
      (revert-buffer t t t))))

;;;;;;;;;;
;; KEYS ;;
;;;;;;;;;;

;; NOTE: prevent org-mode, tex-mode, and other trash
;;       from overriding my beautiful keybindings.
(defun original-forward-paragraph ()
  (interactive)
  (call-interactively 'forward-paragraph))
(defun original-backward-paragraph ()
  (interactive)
  (call-interactively 'backward-paragraph))

(global-set-key (kbd "M-e") 'original-forward-paragraph)
(global-set-key (kbd "M-a") 'original-backward-paragraph)

(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-^") 'enlarge-window)

(global-set-key (kbd "M-<up>")  'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "C-z") nil)

;; (self-insert-command 1 ?\newline)
;; (newline)

;; for terminal compatibilty
(global-set-key (kbd "C-x ;") 'comment-line)

(defmacro my-bind-all (&rest pairs)
  `(progn
     ,@(mapcar
        (lambda (p)
          (let ((key (car p))
                (val (cdr p)))
            (if (consp (car val))
                `(define-key keymap (kbd ,key) (lambda () (interactive) ,@val))
              `(define-key keymap (kbd ,key) (quote ,(car val))))))
        pairs)))

(defconst my-window-map
  (let ((keymap (make-sparse-keymap)))
    (my-bind-all
     ("M-b"   split-window-below)
     ("M-v"   split-window-right)
     ("M-q"   delete-window)
     ("M-S-q" (quit-window 1))
     ("M-k"   kill-buffer-fast)
     ("M-a"   windmove-left)
     ("M-s"   windmove-down)
     ("M-w"   windmove-up)
     ("M-d"   windmove-right)

     ("e"     buffer-menu)
     ("M-e"   switch-to-buffer)
     ("M-i"   (find-file "."))
     ("M-x"   (save-buffers-kill-terminal t))

     ("M-f"   projectile-find-file)
     ("M-g"   my-grep)
     ("r"     my-revert-buffer)
     ("M-r"   query-replace)

     ("M--"   my-default-font-size-dec!)
     ("M-="   my-default-font-size-inc!)
     ("M-j"   my-git-save)
     ("M-h"   my-git-status)

     ("M-c"   shell-command)
     ("c"     my-term-new)
     ("M-t"   my-term-open-last)
     ("M-l"   my-translate)
     ("M-y"   my-term-taged)
     ("M-u"   my-rerun-compile)
     ("t u"   my-run-last-term-command)
     ("f i"   my-fix-imports)
     )

    ;; terminal for each number
    (defmacro macr (z)
      `(define-key
         keymap
         (kbd
          (format "M-%d" ,z))
         (lambda ()
           (interactive)
           (my-term-indexed ,z))))
    (mapc
     #'(lambda (i) (eval `(macr ,i)))
     (list 1 2 3 4 5 6 7 8 9))

    keymap))

(global-set-key (kbd "M-o") my-window-map)

;;;;;;;;;;;;;;
;; AUTOSAVE ;;
;;;;;;;;;;;;;;

;; disable built-in autosafe
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq make-backup-files nil)

(defun my-select-window-advice
    (orig-fun &rest args)
  (let ((old (current-buffer)))
    (apply orig-fun args)
    (unless (or (= ? (elt (buffer-name (current-buffer)) 0)) ; internal?
                (equal old (current-buffer)))
      (my-save-buffer old))))
(advice-add 'select-window :around 'my-select-window-advice)
(advice-add 'switch-to-buffer :around 'my-select-window-advice)

(condition-case nil
	(add-function :after after-focus-change-function #'my-save-current-buffer)
  (error (message "Adding 'after-focus-change-function hook failed. Your emacs is too old.")))

;;;;;;;;;;;;;;;;;;;;;;;
;; LOOK/TRANSPARENCY ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defconst MY-DEFAULT-TRANSPARENCY 95)

(setq default-frame-alist
      (set-in-alist default-frame-alist 'alpha MY-DEFAULT-TRANSPARENCY))

(defun my-set-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun my-toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (eql (cond ((numberp alpha) alpha)
                   ((numberp (cdr alpha)) (cdr alpha))
                   ;; Also handle undocumented (<active> <inactive>) form.
                   ((numberp (cadr alpha)) (cadr alpha)))
             100)
        (my-set-transparency MY-DEFAULT-TRANSPARENCY)
      (my-set-transparency 100))))

;;;;;;;;;;
;; LOOK ;;
;;;;;;;;;;

(font-lock-add-keywords
 'scheme-mode
 '(("\\('\\([^] (){}[]+\\)\\|\\b[+-]?[0-9]+\\([.][0-9]+\\)?\\b\\|\\b[+-]inf.0\\b\\|\\b[+-]?[0-9]+/[0-9]+\\b\\)" . font-lock-constant-face)
   ("\\(\\bset!\\b\\)" . font-lock-keyword-face)
   ("\\(\\bmap\\b\\|\\bfor-each\\b\\|\\breverese\\b\\|\\b=\\b\\|\\beq\\?\\b\\|\\beqv\\?\\b\\|\\bequal\\?\\b\\|\\beval\\b\\|\\\\b\\|\\bcons\\b\\|\\bpair\\?\\b\\)" . font-lock-builtin-face)))

(setq-default frame-title-format '("%b"))

(defun my-default-font-size-set! (size)
  (set-face-attribute 'default nil :height size))
(defun my-default-font-size ()
  (interactive)
  (face-attribute 'default :height nil))
(defun my-default-font-size-inc! ()
  (interactive)
  (my-default-font-size-set!
   (round (* 1.1 (my-default-font-size)))))
(defun my-default-font-size-dec! ()
  (interactive)
  (my-default-font-size-set!
   (round (* 0.9 (my-default-font-size)))))
(my-default-font-size-set! 180)

(defvar buffer-face-mode-remapping) ;; SUPRESS WARNING

(defconst my-emacs-in-terminal-mode? (not window-system))

(defun my-font-exists-p
    (font)
  "check if font exists"
  (condition-case nil
      (if (null (x-list-fonts font)) nil t)
    (error
     (message "Cannot determine if font '%s' exists" font)
     nil)))

(defconst my-prog-font "Fira Code")

(unless my-emacs-in-terminal-mode?
  (unless (my-font-exists-p my-prog-font)
    (message "No font '%s' found" my-prog-font)))

(defun my-set-prog-face ()
  (unless my-emacs-in-terminal-mode?
    (when (my-font-exists-p my-prog-font)
      (setq buffer-face-mode-remapping
            (face-remap-add-relative 'default `(:family ,my-prog-font))))))

(defun my-prog-mode-hook ()
  (tabs-reset-defaults-local)
  (whitespace-mode 1)
  (line-number-mode t)
  (column-number-mode t)
  (my-set-prog-face)
  ;; (set-input-method "TeX")
  )

(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(add-hook 'conf-mode-hook 'my-prog-mode-hook)

(defconst initial-scroll-preserve-screen-position scroll-preserve-screen-position)
(defconst initial-scroll-conservatively scroll-conservatively)
(defconst initial-maximum-scroll-margin maximum-scroll-margin)
(defconst initial-scroll-margin scroll-margin)

(define-minor-mode my-centered-cursor-mode
  "Minor mode for centered cursor viewing."
  :lighter " Centered"
  :after-hook
  (if my-centered-cursor-mode
      (setq-local scroll-preserve-screen-position t
                  scroll-conservatively 1
                  maximum-scroll-margin 0.5
                  scroll-margin 99999)
    (setq-local scroll-preserve-screen-position initial-scroll-preserve-screen-position
                scroll-conservatively initial-scroll-conservatively
                maximum-scroll-margin initial-maximum-scroll-margin
                scroll-margin initial-scroll-margin)))

;;;;;;;;;;;;;;;
;; LOOK/TABS ;;
;;;;;;;;;;;;;;;

;; tab width
(defconst my-default-tab-width 4)
(setq-default tab-width my-default-tab-width)
(defvar my-global-tab-mode t)

(defun disable-tabs ()
  (interactive)
  (setq-local indent-tabs-mode nil)
  (local-unset-key (kbd "TAB")))
(defun enable-tabs  ()
  (interactive)
  (local-set-key (kbd "TAB") 'self-insert-command)
  (setq-local indent-tabs-mode t))

(defun global-disable-tabs ()
  (interactive)
  (setq my-global-tab-mode nil)
  (tabs-reset-defaults-local))
(defun global-enable-tabs ()
  (interactive)
  (setq my-global-tab-mode t)
  (tabs-reset-defaults-local))

(defun tabs-reset-defaults-local  ()
  (setq-local tab-width my-default-tab-width)
  (cond
   ((memq major-mode
          '(lisp-mode
            lisp-interaction-mode
            emacs-lisp-mode
            scheme-mode
            racket-mode
            guile-mode
            rust-mode
            javascript-mode
            typescript-mode
            agda-mode
            agda2-mode
            html-mode
            mhtml-mode))
    (disable-tabs))
   ((memq major-mode
          '(c-mode
            c++-mode))
    (enable-tabs))))

;; (defun reset-local-tab-with ()
;;   (setq-local tab-width my-default-tab-width))
;; (add-hook 'haskell-mode-hook 'reset-local-tab-width)


;;;;;;;;;;;;;;;;;;;;;;
;; BUILTIN PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;;

(defvar js-indent-level) ;; SUPRESS WARNING
(defvar typescript-indent-level) ;; SUPRESS WARNING

(add-hook
 'javascript-mode-hook
 (lambda ()
   (setq tab-width 2)
   (setq-local tab-width 2)
   (setq js-indent-level 2)
   (setq-local js-indent-level 2)
   ))

(add-hook
 'typescript-mode-hook
 (lambda ()
   (setq tab-width 2)
   (setq-local tab-width 2)
   (setq typescript-indent-level 2)
   (setq-local typescript-indent-level 2)
   ))

;; (org agenda
(defconst MY-MEDIA-DIR (getenv "MY_MEDIA"))
(defconst org-agenda-files
  (condition-case nil
      (list
       (my-path-join MY-MEDIA-DIR "text" "notes" "org" "todo"))
    (error (message "Org-agenda directory does not exist"))))
(customize-set-variable
 'org-agenda-default-appointment-duration 90)
;; org agend)

;; always use minimal dired
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(set-in-alist! auto-mode-alist "\\.jsx\\'" 'javascript-mode)
(set-in-alist! auto-mode-alist "\\.ts\\'" 'typescript-mode)
(set-in-alist! auto-mode-alist "\\.tsx\\'" 'typescript-mode)
(set-in-alist! auto-mode-alist "\\.pl\\'" 'prolog-mode)

;; (mode (latex

(defun JH/remove-electric-indent-mode ()
  ;; disable stupid paragraph separators
  (setq paragraph-start (default-value 'paragraph-start))
  (setq paragraph-separate (default-value 'paragraph-separate))
  ;; disable autoindent
  (electric-indent-local-mode -1))

(add-hook 'LaTeX-mode-hook 'JH/remove-electric-indent-mode)
(add-hook 'tex-mode-hook 'JH/remove-electric-indent-mode)

;; latex) mode)

;; (mode (prolog

(defun my-prolog-hook ()
  ;; disable stupid paragraph separators
  (setq paragraph-start (default-value 'paragraph-start))
  (setq paragraph-separate (default-value 'paragraph-separate))
  (local-unset-key (kbd "M-e"))
  (local-unset-key (kbd "M-a"))
  )

(add-hook 'prolog-mode-hook 'my-prolog-hook)

;; prolog) mode)

;; ido mode
;; alternatives are `iswitchb-mode' (obsolete but faster) and `icomplete-mode'
;; remaps `switch-to-buffer' only
;; use `ido-find-file' command for ido files
(ido-mode 'buffer)

(defun my-c-mode-hook ()
  (c-set-style "user"))

(add-hook 'c-mode-hook 'my-c-mode-hook)

(setq-default c-syntactic-indentation nil)

(global-set-key (kbd "C-x g") 'magit-status)

(defun my-text-mode-hook ()
  (visual-line-mode 1)
  (my-centered-cursor-mode 1))
(add-hook 'text-mode-hook 'my-text-mode-hook)

;;;;;;;;;;;;
;; CUSTOM ;;
;;;;;;;;;;;;

(defun my-temp-project ()
  (interactive)
  (let* ((name (make-temp-name "my-emacs-"))
         (dir (temporary-file-directory))
         (s (my-path-join dir name)))
    (mkdir s)
    (find-file (my-path-join s "main.scm"))))

;;;;;;;;;;;;;
;; COMPILE ;;
;;;;;;;;;;;;;

(defun my-is-compile (win)
  (equal
   'compilation-mode
   (with-current-buffer (window-buffer win)
     major-mode)))

(defun my-rerun-compile ()
  (interactive)
  (let ((current-w (selected-window))
        (term-w
         (get-window-with-predicate #'my-is-compile)))

    (my-save-current-buffer-sync)

    (if
        (or (let ((b (get-window-with-predicate #'my-is-compile)))
              (and b (progn (select-window b) t)))
            (let ((b (get-buffer "*compilation*")))
              (and b (progn (switch-to-buffer-other-window b) t))))

        (revert-buffer nil t t)

      (call-interactively 'compile))

    (select-window current-w)))

(add-hook 'compilation-mode-hook
          #'(lambda ()
              (local-set-key (kbd "C-c C-c") 'kill-compilation)))

;;;;;;;;;;
;; TERM ;;
;;;;;;;;;;

(defconst my-term-prefix "my-term-mt")

(defvar my-term-shell-program
  (or (getenv "SHELL")
      (program-resolve-path "fish")
      (program-resolve-path "bash")))

(defconst my-term-loaded-p nil)

(defvar term-raw-map) ;; SUPRESS WARNING
(defun term-mode () nil) ;; SUPRESS WARNING
(defun term-char-mode () nil) ;; SUPRESS WARNING

(defvar my-term-self-id nil)
(defvar my-term-open-list (list))

(defun my-term-load ()
  "Load ansi-terminal and redefine some variables for it."
  (unless my-term-loaded-p
    (require 'term)

    ;; add my window map to term raw mode as well
    (define-key term-raw-map (kbd "M-o") my-window-map)
    (define-key term-raw-map (kbd "C-c C-y") 'term-paste)

    (setq my-term-loaded-p t)))

(defun my-term-base (bufname &rest arguments)
  (my-term-load)
  (let ((shell-file-name my-term-shell-program))
    (set-buffer
     (apply #'make-term
            (append
             (list
              bufname
              (car arguments)
              nil)
             (cdr arguments))))
    (term-mode)
    (term-char-mode)
    (switch-to-buffer (concat "*" bufname "*"))))

(defun my-term-sentinel-advice
    (orig-fun &rest args)
  "Kill buffer on terminal close"

  (apply orig-fun args)
  (let* ((proc (car args))
         (buffer (process-buffer proc)))
    (when (numberp my-term-self-id)
      (setq my-term-open-list (delete my-term-self-id my-term-open-list))
      (kill-buffer buffer))))

(advice-add 'term-sentinel :around
            #'my-term-sentinel-advice)

(defun my-term-taged (name)
  "Start a terminal-emulator in a new buffer named `name'
The buffer is in Term mode;"
  (interactive (list (read-from-minibuffer "tag: ")))
  (my-term-base name my-term-shell-program))

(defun my-term-unsafe (self-id)
  "Unsafely start or open a term indexed by self-id"
  (let* ((name (number-to-string self-id))
         (bufname (concat my-term-prefix name)))
    (my-term-taged bufname)
    (setq my-term-open-list (cons self-id my-term-open-list))
    (setq-local my-term-self-id self-id)))

(defun my-smallest-missing (L1)
  "Return the smallest number that is not in L1 and is greater than 0."
  (let ((n 1))
    (while (member n L1)
      (setq n (+ 1 n)))
    n))

(defun my-term-get-free-index ()
  (my-smallest-missing my-term-open-list))

(defun my-term-indexed (self-id)
  "Start or open a term indexed by self-id"
  (my-term-unsafe self-id))

(defun my-term-new ()
  "Start a new term indexed by unique index"
  (interactive)
  (my-term-unsafe (my-term-get-free-index)))

(defvar my-term-exe-unique-counter 0)

(defun my-term-exe (cmd)
  "Start a terminal-emulator in a new buffer named `program'
The buffer is in Term mode;"
  (interactive (list (read-from-minibuffer "cmd: ")))
  (setq my-term-exe-unique-counter
        (+ 1 my-term-exe-unique-counter))
  (my-term-base
   (format "exe-%d" my-term-exe-unique-counter)
   "/bin/sh" "-c" cmd))

(defun my-term-open-last ()
  "Focuses on the last opened terminal,
 or creates a new one, if there are not open terminals."
  (interactive)
  (if (null my-term-open-list)
      (my-term-new)
    (my-term-indexed (car my-term-open-list))))

;;;;;;;;;;;;;
;; WT-MODE ;;
;;;;;;;;;;;;;

(defconst wt-font-lock
  `(("\\/\\/.*" . font-lock-comment-face)
    (,(regexp-opt '("and" "or" "global" "local" "do" "pardo" "if" "then" "else" "for" "return" "while") 'words)
     . font-lock-keyword-face)
    ("\\b[0-9]+\\b\\|true\\|false" . font-lock-constant-face)
    ("\\bin\\b" . font-lock-function-name-face)
    ("\\.\\.\\|:=\\|\\[\\|\\]\\|\\+\\|\\*\\|\\^\\|\\/\\|<=\\|>=\\|<\\|>" . font-lock-builtin-face)))

(define-derived-mode wt-mode prog-mode "wt-mode"
  :after-hook (whitespace-mode 0)
  "Major mode for editing Work Time pseudocode"
  (setq font-lock-defaults '((wt-font-lock)))
  (setq comment-start "//")
  (setq comment-end ""))

(setq auto-mode-alist
      (set-in-alist auto-mode-alist "\\.wt$" 'wt-mode))
(provide 'wt-mode)

;;;;;;;;;;;;;;;;;
;; SZCALC-MODE ;;
;;;;;;;;;;;;;;;;;

(defconst szcalc-font-lock
  `((";.*" . font-lock-comment-face)
    ("\|" . font-lock-keyword-face)
    ("->" . font-lock-keyword-face)
    ("@" . font-lock-keyword-face)
    ("\\b[0-9\\./]+\\b\\|\\bTrue\\b\\|\\bFalse\\b" . font-lock-constant-face)
    ("^\\w+:" . font-lock-variable-name-face)
    ("\\b[a-zA-Z]\\b" . font-lock-function-name-face)))

(defconst szcalc-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?\; "<")
    (modify-syntax-entry ?\n ">")
    (modify-syntax-entry ?\" "\"")
    (modify-syntax-entry ?\' "\"")
    (syntax-table))
  "Syntax table for `szcalc-mode'.")

(define-derived-mode szcalc-mode prog-mode "szcalc-mode"
  :after-hook (whitespace-mode 0)
  "Major mode for editing .szcalc rules"
  (setq-local font-lock-defaults '((szcalc-font-lock)))
  (setq-local comment-start ";")
  (setq-local comment-add 1)
  (setq-local comment-start-skip ";+[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local comment-column 40)
  (set-syntax-table szcalc-mode-syntax-table))

(setq auto-mode-alist
      (set-in-alist auto-mode-alist "\\.szcalc$" 'szcalc-mode))
(provide 'szcalc-mode)
