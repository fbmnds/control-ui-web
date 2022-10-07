(defpackage #:control-loop
  (:use #:cl #:sqlite #:parse-float)
  (:local-nicknames (#:dex #:dexador)
                    (#:a #:alexandria)
                    (#:yason #:yason)))

(in-package :control-loop)

(defmacro str+ (&rest rest) `(concatenate 'string ,@rest))
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
       ,@body))

(defparameter *path* (str+ (uiop:getenv "HOME") "/projects/control-ui/"))
(defparameter *paths* (make-hash-table))

(setf (gethash :home *paths*) (user-homedir-pathname))
(setf (gethash :projects *paths*)
      (merge-pathnames #p"projects/" (gethash :home *paths*)))

(load (str+ *path* "secrets/secrets.lisp"))

(defparameter *heating-data*
  (connect (merge-pathnames #p"data/control-ui.db" *path*)))

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
(defun handler (env)
  (setf *env* (raw-body->yson env))
  `(200 nil (,(format nil "~a" env))))

(defun handler- (env)
  (let ((js-hdr '(:content-type "application/javascript"))
        (path (getf env :path-info)))
    (handler-case
        (or
         (route path "/index.html" 200 nil *index*)
         (route path "/js/react.js" 200 js-hdr *react*)
         (route path "/js/react-dom.js" 200 js-hdr *react-dom*)
         (route path "/js/react-bootstrap.js" 200 js-hdr *react-bootstrap*)
         (route path "/js/App.js" 200 js-hdr *app-js*)
         (route path "/css/toggle-switch.css" 200 nil *toggle-switch-css* t)
         (route path "/css/bootstrap.css" 200 nil *bootstrap-css* t)
         (route path "/js/bootstrap-bundle.js" 200 js-hdr *bootstrap-bundle-js*)
         (route path "/assets/favicon.ico"
                200 '(:content-type "image/x-icon") *favicon* t)
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

;; (local-time:timestamp-to-unix (local-time:parse-timestring "2022-10-01T03:00:45"))
;;                                                                       ^
(defun nconvert-to-unix (date)
  (local-time:timestamp-to-unix
   (local-time:parse-timestring (nsubstitute #\T #\Space date))))

