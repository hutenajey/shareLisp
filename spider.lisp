(ql:quickload :drakma)
(ql:quickload :cl-ppcre)


(defun sliver (page)
  (drakma:http-request page))

(defun picklinks (content)
  (mapcar #'(lambda (string) 
	      (subseq string 9 (- (length string) 1)))
	  (cl-ppcre:all-matches-as-strings "<a href=\"http:[^\"]*\"" content)))


(defun create-breed-spider (url)
  (let ((index ())
	(graph ())
	(nest ())
	(remainpage ()))
    (lambda ()
      (labels ((breed-spider (curpage)
		     (if (null curpage)
			 (values index graph nest)
			 (progn (if (not (find curpage nest :test #'string=))
				    (progn (nconc nest curpage)
					   (setq remainpage (union (picklinks (sliver curpage)) remainpage))
					   (print remainpage)))
				(setq curpage (car remainpage) remainpage (cdr remainpage))
				(breed-spider curpage)))))
	(breed-spider url)))))
