;;; gptel-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gptel" "gptel.el" (0 0 0 0))
;;; Generated autoloads from gptel.el

(autoload 'gptel-mode "gptel" "\
Minor mode for interacting with ChatGPT.

This is a minor mode.  If called interactively, toggle the `GPTel
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `gptel-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'gptel-send "gptel" "\
Submit this prompt to ChatGPT.

With prefix arg ARG activate a transient menu with more options
instead.

\(fn &optional ARG)" t nil)

(autoload 'gptel "gptel" "\
Switch to or start ChatGPT session with NAME.

With a prefix arg, query for a (new) session name.

Ask for API-KEY if `gptel-api-key' is unset.

If region is active, use it as the INITIAL prompt. Returns the
buffer created or switched to.

\(fn NAME &optional API-KEY INITIAL)" t nil)

(register-definition-prefixes "gptel" '("gptel-"))

;;;***

;;;### (autoloads nil "gptel-curl" "gptel-curl.el" (0 0 0 0))
;;; Generated autoloads from gptel-curl.el

(autoload 'gptel-curl-get-response "gptel-curl" "\
Retrieve response to prompt in INFO.

INFO is a plist with the following keys:
- :prompt (the prompt being sent)
- :buffer (the gptel buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point.

\(fn INFO &optional CALLBACK)" nil nil)

(register-definition-prefixes "gptel-curl" '("gptel-"))

;;;***

;;;### (autoloads nil "gptel-transient" "gptel-transient.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from gptel-transient.el
 (autoload 'gptel-menu "gptel-transient" nil t)

(register-definition-prefixes "gptel-transient" '("gptel-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gptel-autoloads.el ends here
