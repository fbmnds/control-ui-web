(defpackage #:control-loop
  (:use #:cl #:sqlite #:parse-float #:secrets)
  (:local-nicknames (#:dex #:dexador)
                    (#:a #:alexandria)
                    (#:yason #:yason)))

(in-package :control-loop)

(defmacro str+ (&rest rest) `(concatenate 'string ,@rest))
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
       ,@body))

(defparameter *paths* (make-hash-table))
(defun path (path1 &optional path2)
  (if path2
      (merge-pathnames path2 (gethash path1 *paths*))
      (gethash path1 *paths*)))
(defun set-path (key path) (setf (gethash key *paths*) path))
(set-path :home     (user-homedir-pathname))
(set-path :project  (path :home    #p"projects/control-ui/"))
(set-path :d3js     (path :project #p"js/d3-7.min.js"))
(set-path :plotjs   (path :project #p"js/plot-0.6.min.js"))
(set-path :index    (path :project #p"html/index.html"))
(set-path :indexjs  (path :project #p"js/index.js"))
(set-path :gistemp  (path :project #p"data/gistemp.csv"))
(set-path :heating  (path :project #p"data/heating.csv"))
(set-path :indices  (path :project #p"data/indices.csv"))
(set-path :favicon  (path :project #p"html/favicon.ico"))
(set-path :manifest (path :project #p"html/manifest.json"))

(defparameter *heating-data*
  (connect (merge-pathnames #p"data/control-ui.db" (path :project))))

(defparameter *forever* t)

(defparameter *state-at* nil)
(defparameter *state-duration* (* 12 60 60))

(defparameter *slynk-port* 4006)
(defun slynk-start () (slynk:create-server :port *slynk-port*  :dont-close t))
;;(setf slynk:*use-dedicated-output-stream* nil) 


(defparameter *debug* t)
(defparameter *clack-server-type* :hunchentoot)
(defparameter *clack-server* nil)


(defun clack-start (handler)
  (setf *clack-server*
        (clack:clackup handler
                       :server *clack-server-type*
                       :address "0.0.0.0")))

(defun clack-stop ()
  (prog1
   (clack:stop *clack-server*)
   (setf *clack-server* nil)))

(defun clack-restart (handler)
  (clack-stop)
  (clack-start handler))

 (defun route (env-path path rc hdr body &optional ends-with)
  (when (if ends-with
            (a:ends-with-subseq path env-path)
            (a:starts-with-subseq path env-path))
    (if (pathnamep body)
        `(,rc ,hdr ,body)
        `(,rc ,hdr (,body)))))

(defun raw-body->str (env)
  (let ((raw-body (getf env :raw-body))
        str)
    (loop for line = (read-line raw-body nil nil)
          while line do (push line str))
    (apply (lambda (seq) (concatenate 'string seq)) (nreverse str))))

(defun maybe-convert-to-keyword (js-name)
  (or (find-symbol (string-upcase js-name) :keyword)
      :trash))

(defun raw-body->yson (env)
  (let* ((str (raw-body->str env))
         (yason:*parse-json-arrays-as-vectors* t)
         ;;(yason:*parse-object-key-fn* #'maybe-convert-to-keyword)
         (result (yason:parse str)))
    result))

(defparameter *env* nil)
(defun handler-json (env)
  (setf *env* (raw-body->yson env))
  `(200 nil (,(format nil "~a" env))))

(defun handler (env)
  (let ((js-hdr '(:content-type "application/javascript"))
        (path (getf env :path-info)))
    (handler-case
        (or
         (route path "/index.html" 200 nil (path :index))
         (route path "/js/index.js" 200 js-hdr (path :indexjs))
         (route path "/js/d3-7.min.js" 200 js-hdr (path :d3js))
         (route path "/js/plot-0.6.min.js" 200 js-hdr (path :plotjs))
         (route path "/heating.csv"
                200 '(:content-type "text/csv") (path :heating) t)
         (route path "/indices.csv"
                200 '(:content-type "text/csv") (path :indices) t)
         (route path "/gistemp.csv"
                200 '(:content-type "text/csv") (path :gistemp) t)
         (route path "/favicon.ico"
                200 '(:content-type "image/x-icon") (path :favicon) t)
         (route path "/manifest.json"
                200 '(:content-type "application/json") (path :manifest) t)
         (handler-json env)
         `(404 nil (,(format nil "Path not found~%"))))
      (t (e) (if *debug*
                 `(500 nil (,(format nil "Internal Server Error~%~A~%" e)))
                 `(500 nil (,(format nil "Internal Server Error"))))))))

(defun chat (text)
  (dex:request (format nil
                       (if *debug*
                           "http://localhost:5000/bot~a/sendMessage?chat_id=~a"
                           "https://api.telegram.org/bot~a/sendMessage?chat_id=~a")
                       *bot-token* *chat-id*)
               :method :post
               :headers '(("Content-Type" . "application/json"))
               :content (format nil "{\"text\": \"~a\"}" text)))

(defun chat-now (text)
  (when *state-at*
    (let ((ts (get-universal-time)))
      (when (> ts (+ *state-at* *state-duration*))
        (setf *state-at* ts)
        (chat text)))))

(defun control-loop ()
  (while *forever*
    (chat-now "TBD")
    (sleep 60)))

(defun run-control-loop () (bt:make-thread #'control-loop))

;; f = c * 9 / 5 + 32
;; (f - 32) * 5 / 9 = c
(defun f->c (f) (/ (* 5. (- f 32.)) 9.))
(defun c->f (c) (+ (/ (* c 9.) 5.) 32.))

;; (local-time:timestamp-to-unix (local-time:parse-timestring "2022-10-01T03:00:45"))
;;                                                                       ^
(defun nconvert-to-unix (date)
  (local-time:timestamp-to-unix
   (local-time:parse-timestring (nsubstitute #\T #\Space date))))

