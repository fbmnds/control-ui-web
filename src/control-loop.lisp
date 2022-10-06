(defpackage #:control-loop
  (:use #:cl #:sqlite #:parse-float)
  (:local-nicknames (#:dex #:dexador)
                    (#:a #:alexandria)))

(in-package :control-loop)

(defparameter *path* (str+ (uiop:getenv "HOME") "/projects/control-ui/"))
(load (str+ *path* "secrets/secrets.lisp"))

(defparameter *db* (connect (merge-pathnames #p"data/control-ui.db" *path*)))
(defun db-create ()
  (execute-non-query *db* (str+ "create table heating "
                                "(id integer primary key,"
                                "ts text not null,"
                                "temp float null,"
                                "hum float null,"
                                "state text null)")))
(defparameter *heating-data*)

(defparameter *forever* t)

(defparameter *state-at* nil)
(defparameter *state-duration* (* 12 60 60))

(defparameter *slynk-port* 4006)
(slynk:create-server :port *slynk-port*  :dont-close t)
;;(setf slynk:*use-dedicated-output-stream* nil) 

(defparameter *paths* (make-hash-table))

(setf (gethash :home *paths*) (user-homedir-pathname))
(setf (gethash :projects *paths*)
      (merge-pathnames #p"projects/" (gethash :home *paths*)))


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

(defun handler (env)
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
                       "https://api.telegram.org/bot~a/sendMessage?chat_id=~a"
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

(defclass cmd-section (clog-web-content)
  ((form :accessor form :type clog-form)
   (label :accessor label :type clog-label)
   (text :accessor text :type clog-form-element)))

(defmethod create-cmd-section (body)
  (let ((cmd-section (create-web-content body)))
    (change-class cmd-section 'cmd-section)
    (with-slots (form label text)
        cmd-section
      (setf form (create-form cmd-section))
      (setf label (create-label form
                                :content "Enter command"))
      (setf text (create-form-element form :text
                                      :class "w3-input w3-border"
                                      :label label)))
    cmd-section))

(defclass result-section (clog-web-content)
  ())

(defmethod create-result-section (body)
  (let ((result-section (create-web-content body :class "w3-monospace")))
    (set-border result-section :thin :solid :black)
    (setf (overflow result-section) :scroll)
    result-section))

(defun on-cmd (res-section cmd-section)
  (lambda (_)
    (declare (ignore _))
    (let ((cmd-text (text cmd-section)))
      (handler-case
          (progn
            (setf (inner-html res-section)
                  (format nil "~A<br><span style='color:blue'>~A</span><br>~A"
                          (inner-html res-section)
                          (value cmd-text)
                          (lf-to-br (uiop/run-program:run-program
                                     (value cmd-text)
                                     :force-shell t
                                     :output :string))))
            (setf (scroll-top res-section)
                  (scroll-height res-section)))
        (error (c) (clog-web-alert res-section "Error" c :time-out 5)))
      (setf (value cmd-text) ""))))

(defun on-resize (body cmd-section res-section)
  (lambda (_)
    (declare (ignore _))
    (setf (height res-section) (- (inner-height (window body))
                                  (height cmd-section)))))

(defun on-new-window (body)
  (load-css (html-document body) 
	    "https://unpkg.com/@picocss/pico@latest/css/pico.min.css")
  (clog-web-initialize body)
  (setf (title (html-document body)) (str+ "Control UI - " *hostname*))
  (let ((cmd-section (create-cmd-section body))
        (result-section (create-result-section body)))
    (set-on-submit (form cmd-section)
                   (on-cmd result-section cmd-section))
    (let ((resize (on-resize body cmd-section result-section)))
      (funcall resize nil)
      (set-on-resize (window body) resize))))

(defun run-ui () (initialize 'on-new-window))

(defun run-control-loop () (bt:make-thread #'control-loop))





