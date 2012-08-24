#lang racket

(require racket/bytes)  ; For bytes->string/utf8
(require racket/port)   ; For copy-port
(require file/md5)      ; For md5


;; Given an input port of DOT source, this function renders the source
;; into a .png file in cache-dir, and returns the pathname, or #f in
;; case of any error.  The basename of the .png file is the MD5 digest
;; of the source.  So, if the .png file is already there, there would
;; be no conversion.

(define (graphviz-render in cache-dir)
  (let ((source-buffer (open-output-string)))
    (copy-port in source-buffer)
    (let* ((basename (bytes->string/utf-8
                      (md5 (get-output-string source-buffer))))
           (pathname (build-path cache-dir basename)))
      (cond ((file-exists? (path-add-suffix pathname ".png"))
             (close-output-port source-buffer)
             (path-add-suffix basename ".png"))
            (else
             ;; Because subprocess reads from a file port but not a
             ;; string port, we have to write content in source buffer
             ;; into a file.
             (call-with-output-file (path-add-suffix pathname ".dot")
               (lambda (dot-out-port)
                 (write-string (get-output-string source-buffer) dot-out-port)
                 (close-output-port dot-out-port)
                 (close-output-port source-buffer)
                 (if (invoke-graphviz-dot pathname)
                     (path-add-suffix basename ".png")
                     #f))
               #:exists 'replace))))))


;; Create a subproess to invoke the Graphviz DOT command.  This
;; commands assumes that the input dot source file is pathname+".dot"
;; and writes into pathname+".png".

(define (invoke-graphviz-dot pathname)
  (call-with-input-file (path-add-suffix pathname ".dot")
    (lambda (dot-in-port)
      (call-with-output-file (path-add-suffix pathname ".png")
        (lambda (png-port)
          (let-values (((proc in-p out-p err-p)
                        (subprocess
                         png-port
                         dot-in-port
                         (current-error-port)
                         (find-executable-path "dot")
                         "-Tpng")))
            (subprocess-wait proc)
            ;; Returns whether the subprocess succeeded.
            (cond ((= (subprocess-status proc) 0)
                   #t)
                  (else
                   ;; Deletes the possibly incomplete file.
                   (display "Failed invoking dot. Remove out file."
                            (current-error-port))
                   (delete-file (path-add-suffix pathname ".png"))
                   #f))))
        #:exists 'replace))))


(provide graphviz-render)
