
(require-library "file.ss" "dynext")

(let-values (((collect-path _1 _2) (split-path (collection-path "mzlib"))))
   (let ((installation-path (build-path collect-path "quantlib")))
	 (if (not (directory-exists? installation-path))
		 (make-directory installation-path))
	 (for-each
	  (lambda (source-file)
		(let ((destination-file (build-path installation-path source-file)))
		  (if (file-exists? destination-file)
			  (delete-file destination-file))
		  (copy-file (build-path "quantlib" source-file) 
                             destination-file)))
	  (list "quantlib.ss" 
                (append-extension-suffix "QuantLibc")))))


