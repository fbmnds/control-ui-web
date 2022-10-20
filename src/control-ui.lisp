#|
(defpackage #:control-ui
  (:use #:cl #:clog #:clog-web #:sqlite #:parse-float)
  (:local-nicknames (#:dex #:dexador)))
|#

(in-package :control-ui)

(defparameter *path* (str+ (uiop:getenv "HOME") "/projects/control-ui/"))
(defparameter *www-data* (merge-pathnames #P"www-data" *path*))
;;(load (str+ *path* "secrets/secrets.lisp"))

(defparameter *slynk-port* 4007)
(ignore-errors (slynk:create-server :port *slynk-port*  :dont-close t))
;;(setf slynk:*use-dedicated-output-stream* nil)

(defclass session ()
  ((window :initarg :window :accessor window)
   (dom :initarg :dom :accessor dom)
   (body :initarg :body :accessor body)
   (light-section :initarg :light-section :accessor light-section)
   (plot-section :initarg :plot-section :accessor plot-section)
   (cmd-section :initarg :cmd-section :accessor cmd-section)
   (result-section :initarg :result-section :accessor result-section)))

(defmethod initialize-session ((body clog-body))
  (debug-mode body)
  (setf (width body) 640)
  (clog-web-initialize body)
  (let ((session (make-instance 'session
                                :body body
                                :window (window body)
                                :dom (html-document body)
                                :light-section (create-light-section body)
                                :plot-section (create-plot-section body)
                                :cmd-section (create-cmd-section body)
                                :result-section (create-result-section body))))
    (load-css (dom session) "/css/pico.min.css")
    (setf (title (dom session)) (str+ "Control UI - " (uiop:hostname)))
    (push session *sessions*)
    session))

(defclass light-section (clog-web-content)
  ((form :accessor form :type clog-form)
   (label :accessor label :type clog-label)
   (checkbox :accessor checkbox :type clog-form-element)))

(defmethod create-light-section ((body clog-body))
  (let ((light-section (create-web-content body)))
    (change-class light-section 'light-section)
    (with-slots (form label checkbox)
        light-section
      (setf form (create-form light-section
                              :html-id "light-form"))
      (setf label (create-label form
                                :html-id "light-label"
                                :content "Werkstatt Licht"))
      (setf checkbox (create-form-element form :checkbox
                                          :html-id "light-checkbox"
                                          :label label))
      (setf (attribute checkbox "role") "switch")
      (place-before label checkbox))
    (js-update-light light-section (light-?))
    light-section))

(defmethod js-update-light ((light-section light-section) checked)
  (with-slots (label checkbox)
      light-section
    (cond ((not (numberp checked))
           (remove-attribute checkbox "checked")
           (setf (inner-html label) "Werkstatt Licht *FEHLER*"))
          ((= 0 checked)
           (remove-attribute checkbox "checked")
           (setf (inner-html label) "Werkstatt Licht *AUS*"))
          ((= 1 checked)
           (setf (attribute checkbox "checked") nil)
           (setf (inner-html label) "Werkstatt Licht *AN*")))))

(defmethod on-toggle-light ((light-section light-section))
  (lambda (_)
    (declare (ignore _))
    (let ((checked (light-toggle)))
      (cond ((not (numberp checked)) (js-execute light-section "lightError();"))
            ((= 0 checked) (js-execute light-section "lightOff();"))
            ((= 1 checked) (js-execute light-section "lightOn();"))))))

(defclass plot-section (clog-web-content)
  ((data :accessor data)))

(defmethod js-update-data ((plot-section plot-section) data)
  (js-execute plot-section (format nil "data = ~a; setData(data);" data)))

(defmethod create-plot-section ((body clog-body))
  (let ((plot-section (create-web-content body :html-id "plot")))
    (change-class plot-section 'plot-section)
    (set-border plot-section :thin :solid :black)
    (set-geometry plot-section :width 640)
    (load-script (html-document body)
                 "/js/d3-7.min.js" :wait-for-load t)
    (load-script (html-document body)
                 "/js/plot-0.6.min.js" :wait-for-load t)
    (load-script (html-document body)
                 "/js/temperature.js" :wait-for-load t)
    plot-section))

(defmethod plot-data ((plot-section plot-section))
  (setf *sessions*
        (remove-if-not (lambda (session) (connection-body (window session)))
                       *sessions*))
  (let ((data (uiop:run-program (str+ "/usr/bin/sqlite3 -json "
                                      *control-ui-db*
                                      " 'select * from heating"
                                      " order by ts desc"
                                      " limit 800;'")
                                :output '(:string :stripped t))))
    (js-update-data plot-section data)))

(defclass cmd-section (clog-web-content)
  ((form :accessor form :type clog-form)
   (label :accessor label :type clog-label)
   (text :accessor text :type clog-form-element)))

(defmethod create-cmd-section ((body clog-body))
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

(defmethod create-result-section ((body clog-body))
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

(defmethod on-resize ((body clog-body) cmd-section res-section)
  (lambda (_)
    (declare (ignore _))
    (setf (height res-section) (- (inner-height (window body))
                                  (height cmd-section)))))

(defmethod set-html-on-close ((_1 clog-body) _2)
  (declare (ignore _1 _2))
  (setf *sessions*
        (remove-if-not (lambda (session) (connection-body (window session)))
                       *sessions*)))

(defmethod on-index ((body clog-body))
  (let ((session (initialize-session body)))
    (with-slots (window body
                 light-section plot-section cmd-section result-section)
        session
      (js-update-light light-section (light-?))
      (plot-data plot-section)
      (set-on-click (form light-section)
                    (on-toggle-light light-section))
      (set-on-submit (form cmd-section)
                     (on-cmd result-section cmd-section))
      (let ((resize (on-resize body cmd-section result-section)))
        (funcall resize nil)
        (set-on-resize window resize))
      (run body))))

(defun run-ui ()
  (initialize 'on-index :static-root *www-data*)
  (set-on-new-window 'on-index :path "/index.html"))

;;(defun run-control-loop () (bt:make-thread #'control-loop))






