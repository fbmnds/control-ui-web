
(in-package :control-ui)

(defparameter *sessions* nil)

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
  (and (clack-stop)
       (clack-start handler)))

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
  (ignore-errors
   (let* ((str (raw-body->str env))
          (yason:*parse-json-arrays-as-vectors* t)
          ;;(yason:*parse-object-key-fn* #'maybe-convert-to-keyword)
          (result (yason:parse str)))
     result)))

;; "2022-10-10 20:49:47 17.8*C 50.8% IDLE"
;;  0-18 20-23 27-30 33+
(defparameter *env* nil)

(defun handler-json (env)
  (handler-case
      (progn
        (setf *env* env)
        (let* ((db (sqlite:connect *control-ui-db*))
               (msg (gethash "text" (raw-body->yson env)))
               (ts (subseq msg 0 19))
               (temp (subseq msg 20 24))
               (hum (subseq msg 27 31))
               (state (subseq msg 33))
               (data (str+ "{ 'ts': '" ts "',"
                           " 'temp': " temp ","
                           " 'hum': " hum ","
                           " 'state': '" state "' }")))
          (execute-non-query db
                             (str+ "insert into heating (ts,temp,hum,state)"
                                   " values (?,round(?,2),round(?,2),?)")
                             ts temp hum state)
          (let ((checked (light-?)))
            (dolist (s *sessions*)
              (plot-data (plot-section s))
              (js-update-light (light-section s) checked))))
        `(200 nil ("ok")))
    (t (e) (if *debug*
                 `(500 nil (,(format nil "Internal Server Error~%~A~%" e)))
                 `(500 nil (,(format nil "Internal Server Error")))))))

