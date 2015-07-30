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

(defun cleartags(content)
  (cl-ppcre:regex-replace-all
   "&.{1,5};|&#.{1,5};"
   (cl-ppcre:regex-replace-all
    "<[\\d\\D]*?>"
    (cl-ppcre:regex-replace-all
     "<script[\\d\\D]*?>[\\d\\D]*?<\/script>"
     (cl-ppcre:regex-replace-all
      "<style[\\d\\D]*?>[\\d\\D]*?<\/style>" content
      "")
     "")
    "")
   ""))

(defun content-split-trim (content ltrim)
  (mapcar #'(lambda (line)
	      (string-trim ltrim line))
	  (cl-ppcre:split "\\n+" content)))

(defun addprevN (num list)
  (mapcon #'(lambda (cur)
	      (if (< (length cur) num)
		  
		  (loop for x across (subseq cur 0 num) summing x)))
	  list))

(defun pickMainContent (content block-line-num)
  (let* ((lines (content-split-trim content '(#\Space #\Tab #\Newline)))
	 (blocklengths (addprevN block-line-num (mapcar #'length lines))))
    
    
  )

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
