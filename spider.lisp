(ql:quickload :drakma)
(ql:quickload :cl-ppcre)
(ql:quickload :closure-html)
(ql:quickload :cxml-stp)

(defun sliver (page)
  (handler-case (drakma:http-request page :connection-timeout 5)
    (condition nil)))

(defun picklinks (content)
  (let ((result (list "result")))
    (when (not (null content))
      (stp:do-recursively (a (chtml:parse content (cxml-stp:make-builder)))
	(when (and (typep a 'stp:element)
		   (equal (stp:local-name a) "a"))
	  (nconc result (cons (stp:attribute-value a "href") nil)))))
    (cdr result)))


(defun create-breed-spider (url)
  (let ((index ())
	(graph ())
	(nest ())
	(remainpage ()))
    (lambda ()
      (labels ((breed-spider (curpage)
		 (if (null curpage)
		     (values index graph nest)
		     (progn (when (not (find curpage nest :test #'string=))
			      (nconc nest curpage)
			      (setq remainpage
				    (union (picklinks (sliver curpage)) remainpage))
			      (print remainpage))
			    (setq curpage (pop remainpage))
			    (breed-spider curpage)))))
	(breed-spider url)))))
