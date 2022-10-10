(in-package #:control-ui)

(defun fmt-rfc3339-now ()
  (local-time:format-rfc3339-timestring nil (local-time:now)))

(defun last-backup ()
  (let ((cmd (str+ "/bin/ls " *backup-dir*
                   " | /usr/bin/sort | /usr/bin/tail -n 1")))
    (string-trim '(#\Linefeed #\Newline)
                 (str+ *backup-dir*
                       (uiop:run-program cmd :force-shell t :output :string)))))

(defun select-last-ts ()
  (let ((cmd (str+ "/usr/bin/sqlite3 "
                   (last-backup)
                   " 'select ts from heating order by ts desc limit 1;'")))
    (uiop:run-program cmd :force-shell t
                          :output '(:string :stripped t))))

(defun fetch-delta ()
  (ignore-errors
   (let* ((last-ts (select-last-ts))
          (cmd (str+ "/usr/bin/ssh " *ssh-prod-parameter*
                     "\"/usr/bin/sqlite3 -json "
                     *prod-file*
                     " 'select * from heating"
                     " where ts > \\\"" last-ts "\\\""
                     " order by ts asc;'\""))
          (delta (uiop:run-program cmd :force-shell t
                                       :output '(:string :stripped t))))
     (when (stringp delta) (yason:parse delta)))))

(defun run-fetch-insert-delta ()
  (let ((delta (fetch-delta))
        (db (str+ *backup-dir* (fmt-rfc3339-now) "-heating.db")))
    (handler-case
         (progn
           (uiop:run-program (str+ "/bin/cp " (last-backup) " " db)
                             :force-shell t)
           (let ((connected-db (sqlite:connect db)))
             (dolist (i delta)
               (execute-non-query
                connected-db (str+ "insert into heating (ts,temp,hum,state)"
                                   " values (?,round(?,2),round(?,2),?)")
                (gethash "ts" i) (gethash "temp" i)
                (gethash "hum" i) (gethash "state" i))))
           db)
      (error (c)
        (uiop:run-program (str+ "/bin/rm " db) :force-shell t)
        c))))

(defun run-sshfs-backup-data ()
  (let* ((db-file-name
           (str+ *backup-dir* (fmt-rfc3339-now) "-heating.db"))
         (cp (str+ "/bin/cp " *sshfs-prod-file* " " db-file-name)))
    (multiple-value-bind (_1 rc _2)
        (uiop:run-program cp :force-shell t)
      (if rc (values _1 rc _2) db-file-name))))



