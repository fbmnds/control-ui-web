(defpackage #:control-ui
  (:use #:cl #:clog #:clog-web #:sqlite #:parse-float)
  (:local-nicknames (#:dex #:dexador)))

(in-package :control-ui)

(defmacro str+ (&rest rest) `(concatenate 'string ,@rest))
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
       ,@body))

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

(defparameter *forever* t)

(defparameter *state-at* nil)
(defparameter *state-duration* (* 12 60 60))

(defparameter *slynk-port* 4007)
(ignore-errors (slynk:create-server :port *slynk-port*  :dont-close t))
;;(setf slynk:*use-dedicated-output-stream* nil) 

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

;;(defun run-control-loop () (bt:make-thread #'control-loop))






