(defpackage #:biz.naiz.web
#+allegro  (:use :excl #:cl #:net.aserve #:net.html.generator)
#+(or sbcl clisp ccl) (:use #:cl #:acl-compat.excl #:net.aserve #:net.html.generator))
  ;(:use #:cl #:net.aserve #:net.html.generator))

(in-package #:biz.naiz.web)

(defmacro release-function (release-path  title &body body)
 `(publish
     :path ,release-path
     :content-type "text/html;charset=utf-8"
     :function
     #'(lambda (req ent)
         (with-http-response (req ent)
             (with-http-body (req ent :external-format (crlf-base-ef :utf-8))
                 (html (:html (:head (:title ,title)
                                     ((:link :rel "stylesheet" :href "/skin/main.css")))
                              ,@body)))))))

(defmacro load-page (path)
  `(html 
     (:body (:h3 "上传文件共享")
            (:hr ((:form :action "/upload" :method "post" :enctype "multipart/form-data")
                  ((:input :type "file" :name "uploadfile" :id "file")) :br
                  ((:input :type "submit" :name "button" :value "上传")))
                 (:h3 "共享的文件") (:hr 
                   (list-directory-to-html ,path))))))



(defun is-directory (path file)
  (probe-file (concatenate 'string path  file "/")))

(defun get-file-image (file)
 (let ((image (concatenate 'string "/image/" (pathname-type file) ".gif")))
   (if (probe-file (concatenate 'string "~/www" image))
     image
     "/image/alert.black.gif")))

(defun get-directory-list (path)
  (directory (concatenate 'string path "*.*")))


(defun list-directory-to-html (path-input)
  (let* ((path (concatenate 'string path-input "/"))
          (res-path (concatenate 'string "~/www/res" path)))
     (html (:ul
             (dolist (file (get-directory-list res-path))
               (let* ((item (file-namestring file))
                      (image (if (is-directory res-path item)
                               (let ((new-path (concatenate 'string path item)))
                                 (release-function new-path new-path (load-page new-path))
                                 "/image/dir.gif")
                               (get-file-image item))))
                 (html (:li ((:img :src image))
                            ((:a :href (concatenate 'string path  item)) (:princ item))))))))))


(release-function "/" "LZY Could" (load-page ""))
(release-function "/image" "Image List"
  (:body (:ul
      (dolist (file (directory "~/www/image/*.*"))
        (html (:li ((:img :src (concatenate 'string "/image/" (file-namestring file))))))))))


(publish-directory :prefix "/" :destination "/home/lzy/www/res/" :hook #'(lambda (req ent extra) (format t "~A,~A,~A~%" req ent extra)))
(publish-directory :prefix "/skin" :destination "/home/lzy/www/skin/")
(publish-directory :prefix "/image" :destination "/home/lzy/www/image/")
(publish-file :path "/favicon.ico" :file "/home/lzy/www/image/favicon.ico")

(start :port 8080 )

(publish :path "/upload"
 :content-type "text/html;charset=utf-8"
 :function
#'(lambda (req net)
    (with-http-response (req net)
     (let ((files-written)
           (text-strings)
           (overlimit))
      (loop
       (multiple-value-bind (kind name filename content-type)
        (parse-multipart-header
         (get-multipart-header req))
        (case kind
         (:eof (return))
         (:data (push (cons name (get-all-multipart-data req))
                 text-strings))
         (:file (let ((contents (get-all-multipart-data req
                                :type :binary
                                :limit 1000000000)))
                 (let ((seq (max (or (position #\/ filename
                                      :from-end t) -1)
                             (or (position #\\ filename
                                  :from-end t) -1))))
                  (if* seq
                   then (setq filename (subseq filename (1+ seq)))))
                 (if* (eq contents :limit)
                  then (setq overlimit t)
                  elseif (equal filename "")
                  thenret
                  else
				  (with-open-file (p (merge-pathnames  filename "/home/lzy/www/res/")
						   :direction :output
						   :if-exists :supersede
						   :element-type '(unsigned-byte 8))
				    (format 
				     t "writing file ~s, content-type ~s~%"
				     filename content-type)
				    (push filename files-written)
				    (write-sequence contents p)))))
         (t ; all else ignore but read to next header
           (get-all-multipart-data req :limit 1000000)))))
     (with-http-body (req net :external-format :utf8-base)
                     (html (:head (:title "upload file"))
                           (:body
                             "文件上传成功!"
                             ((:a :href "/") "查看共享文件列表"))))))))
