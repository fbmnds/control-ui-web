(defpackage #:control-ui
  (:use #:cl #:clog #:clog-web #:sqlite #:parse-float)
  (:local-nicknames (#:dex #:dexador)))

(in-package :control-ui)

(defmacro str+ (&rest rest) `(concatenate 'string ,@rest))
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
       ,@body))

(defparameter *path* (str+ (uiop:getenv "HOME") "/a64/projects/control-ui/"))
(defparameter *www-data* (merge-pathnames #P"www-data" *path*))
;;(load (str+ *path* "secrets/secrets.lisp"))

(defparameter *slynk-port* 4007)
(ignore-errors (slynk:create-server :port *slynk-port*  :dont-close t))
;;(setf slynk:*use-dedicated-output-stream* nil)

(defparameter *window* nil)
(defparameter *index-body* nil)
(defparameter *plot-section* nil)
(defparameter *cmd-section* nil)
(defparameter *result-section* nil)


(defclass plot-section (clog-web-content)
  ((data :accessor data)))

(defmethod js-query-data ((plot-section plot-section))
  (setf (data plot-section) (js-query plot-section "data")))

(defmethod create-plot-section (body)
  (flet ((create-section (body)
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
         (create-section-after (plot-section)
           (setf (data plot-section) (js-query-data plot-section))
           plot-section))
    (create-section-after (create-section body))))

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

(defun on-index (body)
  (setf *index-body* body)
  (setf (width body) 640)
  (debug-mode body)
  (load-css (html-document body) "/css/pico.min.css")
  (clog-web-initialize body)
  (setf (title (html-document body)) (str+ "Control UI - " *hostname*))
  (let ((plot-section (create-plot-section body))
        (cmd-section (create-cmd-section body))
        (result-section (create-result-section body)))
    (js-query-data plot-section)
    (setf *plot-section* plot-section)
    (setf *cmd-section* cmd-section)
    (setf *result-section* result-section)
    (set-on-submit (form cmd-section)
                   (on-cmd result-section cmd-section))
    (let ((resize (on-resize body cmd-section result-section)))
      (funcall resize nil)
      (set-on-resize (window body) resize))))

(defun run-ui ()
  (initialize 'on-index :static-root *www-data*)
  (set-on-new-window 'on-index :path "/index.html"))

;;(defun run-control-loop () (bt:make-thread #'control-loop))






