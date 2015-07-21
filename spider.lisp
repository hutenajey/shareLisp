(ql:quickload :drakma)
(ql:quickload :cl-ppcre)


(defun sliver (page)
  (drakma:http-request page))

(defun picklinks (content)
  (mapcar #'(lambda (string) 
	      (subseq string 9 (- (length string) 1)))
	  (cl-ppcre:all-matches-as-strings "<a href=\"http[^\"]*\"" content)))


(defun breed-spider (url)
  (let ((index ())
	(graph ())
	(nest ()))
    (do* ((page url (car remainpage))
	  (remainpage nil (cdr remainpage)))
	((null page) (values index graph nest))
      (when (not (find page nest :test #'string=))
	(let* ((content (sliver page))
	       (links (picklinks content)))
	  (setq remainpage (union links remainpage))
	  (print links)
	  (print remainpage)
	  (nconc nest page))))))
	  
