
(define-module (my helpers)
  :export (
           ~a
           package-list
           fail-if-package-not-found?
           get-package+output
           define-packages
           filter-func
           )
  )

(use-modules (gnu))
(use-modules (gnu packages))
(use-modules (gnu packages commencement))
(use-modules (guix profiles))

(define fail-if-package-not-found?
  (make-parameter #t))

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

(define-syntax-rule (package-list . args)
  (filter filter-func
          (map (compose list get-package+output symbol->string)
               (quote args))))

(define-syntax-rule (define-packages name . pkgs)
  (define name (package-list . pkgs)))
