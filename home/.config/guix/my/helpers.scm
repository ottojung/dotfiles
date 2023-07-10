
(define-module (my helpers)
  :export (
           patch-pkglist
           ~a
           fail-if-package-not-found?
           get-package+output
           define-packages
           filter-func
           symbols->manifest
           )

  :use-module (gnu)
  :use-module (gnu packages)
  :use-module (gnu packages commencement)
  :use-module (guix profiles)
  )


(define fail-if-package-not-found?
  (make-parameter #t))

(define (package-group? quoted-name)
  (hash-ref package-groups quoted-name #f))

(define-syntax patch-pkglist
  (syntax-rules (+ -)
    ((_ + pkgname . rest)
     (patch-pkglist pkgname . rest))
    ((_ original + pkgname . rest)
     (patch-pkglist
      (let ((pkgname* (quote pkgname)))
        (if (package-group? pkgname*)
            (append pkgname original)
            (cons pkgname* original)))
      . rest))
    ((_ original - pkgname . rest)
     (let ((pkgname* (quote pkgname)))
       (patch-pkglist
        (if (package-group? pkgname*)
            (delete-list pkgname original)
            (delete pkgname* original))
        . rest)))
    ((_ original) original)))

(define (delete-list to-remove original)
  (filter
   (lambda (elem)
     (not (member elem to-remove)))
   original))

(define (~a x)
  (cond
   ((string? x) x)
   ((number? x) (number->string x))
   ((symbol? x) (symbol->string x))
   (else
    (with-output-to-string
      (lambda _
        (display x))))))

(define filter-func
  (lambda (p)
    (if (fail-if-package-not-found?)
        (unless (car p)
          (display "Exiting as promised\n" (current-error-port))
          (display "Check fail-if-package-not-found? flag\n" (current-error-port))
          (exit 1))
        (and (car p) p))))

(define (get-package+output name)
  (write name) (newline)

  (catch #t
         (lambda _ (specification->package+output name))
         (lambda err
           (display
            (string-append
             "Cannot find package '"
             name
             "' because of "
             (~a err)
             ". This package will be skipped!\n")
            (current-error-port))
           (values #f #f))))


(define (symbols->packages symboled-names)
  (parameterize ((current-output-port (current-error-port)))
    (write symboled-names) (newline))

  (filter filter-func
          (map (compose list get-package+output symbol->string)
               symboled-names)))


(define (symbols->manifest symboled-names)
  (packages->manifest (symbols->packages symboled-names)))


(define package-groups
  (make-hash-table))


(define-syntax-rule (define-packages name . pkgs)
  (begin
    (hash-set! package-groups (quote name) (quote pkgs))
    (define name (quote pkgs))))
