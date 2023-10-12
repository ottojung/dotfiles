;;; seqel-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "seqel" "seqel.el" (0 0 0 0))
;;; Generated autoloads from seqel.el

(register-definition-prefixes "seqel" '("seqel-"))

;;;***

;;;### (autoloads nil "seqel-fasta-mode" "seqel-fasta-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from seqel-fasta-mode.el

(add-to-list 'auto-mode-alist '("\\.\\(fasta\\|fa\\|fna\\|faa\\|aln\\)\\'" . seqel-fasta-mode))

(autoload 'seqel-fasta-mode "seqel-fasta-mode" "\
Major mode for editing biological sequences in fasta format.

Special commands:
\\{seqel-fasta-mode-map}
\\{seqel-nuc-mode-map}
\\{seqel-pro-mode-map}

\(fn)" t nil)

(autoload 'seqel-fasta-guess-on-load "seqel-fasta-mode" "\
Whether to enable `seqel-fasta-mode' by guessing buffer content." t nil)

(autoload 'seqel-fasta-forward "seqel-fasta-mode" "\
Move forward to the end fasta record.

It works in the style of `forward-paragraph'.  Count need to be
positive integer.  Return current point if it moved over COUNT of
records; otherwise return nil.

\(fn COUNT)" t nil)

(autoload 'seqel-fasta-last "seqel-fasta-mode" "\
Go to the beginning of last fasta record." t nil)

(autoload 'seqel-fasta-first "seqel-fasta-mode" "\
Go to the beginning of first fasta record." t nil)

(autoload 'seqel-fasta-count "seqel-fasta-mode" "\
Count the number of fasta sequences in the buffer." t nil)

(autoload 'seqel-fasta-mark "seqel-fasta-mode" "\
Put point at the beginning of the sequence and mark the end.

If a prefix arg is provided or INCLUDE-HEADER is t, then put the point at
the beginning of the fasta entry instead of the sequence.

\(fn &optional INCLUDE-HEADER)" t nil)

(autoload 'seqel-fasta-format "seqel-fasta-mode" "\
Format the current sequence to contain WIDTH chars per line.

It wraps around `seqel-fasta--format'.  This can take ~10
seconds for long sequences (> 5M base pairs).  If WIDTH is
nil, each fasta sequence will be formatted into a single line.

\(fn &optional WIDTH)" t nil)

(autoload 'seqel-fasta-rc "seqel-fasta-mode" "\
Reverse complement current DNA/RNA sequence.

It wraps on `seqel-fasta--rc'." t nil)

(autoload 'seqel-fasta-translate "seqel-fasta-mode" "\
Translate the current fasta sequence to amino acids.

It is just a wrapper on `seqel-fasta--translate'." t nil)

(register-definition-prefixes "seqel-fasta-mode" '("seqel-fasta-"))

;;;***

;;;### (autoloads nil "seqel-genbank-mode" "seqel-genbank-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from seqel-genbank-mode.el

(add-to-list 'auto-mode-alist '("\\.\\(genbank\\|gb\\|gbk\\)\\'" . seqel-genbank-mode))

(autoload 'seqel-genbank-mode "seqel-genbank-mode" "\
Major mode for editing sequences in genbank format.

Special commands:
￼\\{seqel-genbank-mode-map}
  \\{seqel-nuc-mode-map}
  \\{seqel-pro-mode-map}

\(fn)" t nil)

(register-definition-prefixes "seqel-genbank-mode" '("seqel-genbank-"))

;;;***

;;;### (autoloads nil "seqel-genetic-code" "seqel-genetic-code.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from seqel-genetic-code.el

(register-definition-prefixes "seqel-genetic-code" '("seqel-genetic-code-table"))

;;;***

;;;### (autoloads nil "seqel-nuc-mode" "seqel-nuc-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from seqel-nuc-mode.el

(autoload 'seqel-nuc-move-forward "seqel-nuc-mode" "\
Move forward COUNT bases.  Move backward if negative.

Skip `seqel-cruft-regexp' but stop on the illegal base
and report how many bases the point have been moved by.
COUNT can be either positive or negative, indicating the
moving direction.  Return the number of bases that are moved thru.
See `seqel-forward-char'

\(fn COUNT)" t nil)

(autoload 'seqel-nuc-move-backward "seqel-nuc-mode" "\
Move backward COUNT bases, similar to `seqel-nuc-move-forward'.

See also `seqel-forward-char'.  `(seqel-nuc-move-backward -1)'
and `(seqel-nuc-move-forward 1)' are equvialent.

\(fn COUNT)" t nil)

(autoload 'seqel-nuc-reverse-complement "seqel-nuc-mode" "\
Reverse complement a region of DNA or RNA sequence.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active.

See also `seqel-nuc-complement'.

\(fn BEG END)" t nil)

(autoload 'seqel-nuc-2rna "seqel-nuc-mode" "\
Convert the region or the current line to RNA.

It basically converts 't' -> 'u' and 'T' -> 'U'.  Interactively,
BEG and END are the begin and end of the active region or the
current line if no region is active.  If NEGATE is not nil,
convert to DNA.  See also `seqel-nuc-2dna'.

\(fn BEG END &optional NEGATE)" t nil)

(autoload 'seqel-nuc-paint "seqel-nuc-mode" "\
Color the nucleic acid region BEG to END.

If CASE is nil, upcase and lowercase base chars will be colored the same;
otherwise, not.  See `seqel-paint' for details.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active.

\(fn BEG END &optional CASE)" t nil)

(defalias 'seqel-nuc-unpaint 'seqel-unpaint "\
Uncolor the nucleotide sequence region.

It is an alias to `seqel-unpaint'.")

(autoload 'seqel-nuc-translate "seqel-nuc-mode" "\
Translate the nucleotides to protein using current translation table.

The ambiguous codon will be handled correctly: if it is mapped to
multiple amino acids, 'X' will be the output.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active.

This function translates DNA of 9K for ~6 sec (over 80% of the
time is for `seqel-nuc-decode') and ~15 MB mem.  It is not super fast,
but it is very rare you need to translate a sequence over 10K
long.

\(fn BEG END)" t nil)

(register-definition-prefixes "seqel-nuc-mode" '("seqel-nuc-"))

;;;***

;;;### (autoloads nil "seqel-pro-mode" "seqel-pro-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from seqel-pro-mode.el

(autoload 'seqel-pro-move-forward "seqel-pro-mode" "\
Move forward COUNT number of amino acids.

See `seqel-nuc-move-forward'

\(fn COUNT)" t nil)

(autoload 'seqel-pro-paint "seqel-pro-mode" "\
Color the protein sequence from BEG to END.

If the CASE is nil, upcase and lowercase base chars will be colored the same;
otherwise, not.  See `seqel-paint' for details.

\(fn BEG END &optional CASE)" t nil)

(defalias 'seqel-pro-unpaint 'seqel-unpaint "\
Uncolor the region of protein sequence.

This is an alias to `seqel-unpaint'.")

(register-definition-prefixes "seqel-pro-mode" '("seqel-pro-"))

;;;***

;;;### (autoloads nil nil ("seqel-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; seqel-autoloads.el ends here
