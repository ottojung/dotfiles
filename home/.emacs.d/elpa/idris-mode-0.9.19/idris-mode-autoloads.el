;;; idris-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "idris-ipkg-mode" "idris-ipkg-mode.el" (23602
;;;;;;  26059 410720 786000))
;;; Generated autoloads from idris-ipkg-mode.el

(autoload 'idris-ipkg-mode "idris-ipkg-mode" "\
Major mode for Idris package files
     \\{idris-ipkg-mode-map}
Invokes `idris-ipkg-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "idris-mode" "idris-mode.el" (23602 26059 406720
;;;;;;  667000))
;;; Generated autoloads from idris-mode.el

(autoload 'idris-mode "idris-mode" "\
Major mode for Idris
     \\{idris-mode-map}
Invokes `idris-mode-hook'.

\(fn)" t nil)

(push '("\\.idr$" . idris-mode) auto-mode-alist)

(push '("\\.lidr$" . idris-mode) auto-mode-alist)

;;;***

;;;### (autoloads nil "idris-simple-indent" "idris-simple-indent.el"
;;;;;;  (23602 26059 386720 77000))
;;; Generated autoloads from idris-simple-indent.el

(autoload 'idris-simple-indent-mode "idris-simple-indent" "\
Simple Idris indentation mode that uses simple heuristic.
In this minor mode, `indent-for-tab-command' (bound to <tab> by
default) will move the cursor to the next indent point in the
previous nonblank line, whereas `idris-simple-indent-backtab'
\(bound to <backtab> by default) will move the cursor the
previous indent point. An indent point is a non-whitespace
character following whitespace.

Runs `idris-simple-indent-hook' on activation.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-idris-simple-indent "idris-simple-indent" "\
Turn on function `idris-simple-indent-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("idris-commands.el" "idris-common-utils.el"
;;;;;;  "idris-compat.el" "idris-core.el" "idris-events.el" "idris-highlight-input.el"
;;;;;;  "idris-hole-list.el" "idris-info.el" "idris-keys.el" "idris-log.el"
;;;;;;  "idris-mode-pkg.el" "idris-prover.el" "idris-repl.el" "idris-settings.el"
;;;;;;  "idris-syntax.el" "idris-tree-info.el" "idris-warnings-tree.el"
;;;;;;  "idris-warnings.el" "inferior-idris.el") (23602 26059 414720
;;;;;;  903000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; idris-mode-autoloads.el ends here
