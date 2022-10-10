(in-package #:control-ui)

(defun fmt-rfc3339-now ()
  (local-time:format-rfc3339-timestring nil (local-time:now)))

(defun select-last-ts-cmd ()
  (str+ "/usr/bin/sqlite3 "
        *backup-file*
        " 'select ts from heating order by ts desc limit 1;'"))

(defun run-select-last-ts ()
  (let ((cmd (select-last-ts-cmd)))
    (uiop:run-program cmd :force-shell t
                          :output '(:string :stripped t))))

(defun fetch-ssh-cmd (last-ts)
  (str+ "/usr/bin/ssh " *ssh-prod-parameter*
        "\"/usr/bin/sqlite3 -json "
        *prod-file*
        " 'select * from heating"
        " where ts > \\\"" last-ts "\\\""
        " order by ts asc;'\""))

(defun fetch-delta ()
  (ignore-errors
   (let* ((last-ts (run-select-last-ts))
          (cmd (fetch-ssh-cmd last-ts))
          (delta (uiop:run-program cmd :force-shell t
                                       :output '(:string :stripped t))))
     (when (stringp delta) (yason:parse delta)))))

(defun insert-delta ()
  (let ((delta (fetch-delta))
        (db (str+ *backup-dir* (fmt-rfc3339-now) "-heating.db")))
    (handler-case
        (progn
          (uiop:run-program (str+ "/bin/cp " *backup-file* " " db))
          (let ((connected-db (sqlite:connect db)))
            (dolist (i delta)
              (ignore-errors
               (execute-non-query
                connected-db (str+ "insert into heating (ts,temp,hum,state)"
                                   " values (?,round(?,2),round(?,2),?)")
                (gethash "ts" i) (gethash "temp" i)
                (gethash "hum" i) (gethash "state" i))))))
      (error (c) c))))

(defun run-sshfs-backup-data ()
  (let* ((db-file-name
           (str+ *backup-dir* (fmt-rfc3339-now) "-heating.db"))
         (cp (str+ "/bin/cp " *sshfs-prod-file* " " db-file-name)))
    (multiple-value-bind (_1 rc _2)
        (uiop:run-program cp :force-shell t)
      (if rc
          (values _1 rc _2)
          (setf *backup-file* db-file-name)))))



