
(define (find-install-path path)
  (if (null? path) 
      #f
      (let ((dir (car path))
            (cwd (getcwd)))
        (if (and (file-exists? dir) 
                 (file-is-directory? dir)
                 (not (string=? dir "."))
                 (not (string=? dir cwd)))
            dir
            (find-install-path (cdr path))))))

(let ((install-path (find-install-path %load-path)))
  (for-each (lambda (file)
              (system (format #f "install -m 0555 ~A ~A"
                                 file
                                 (string-append install-path
                                                "/"
                                                file))))
            '("QuantLib.scm" "QuantLibc.so")))
