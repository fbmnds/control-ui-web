(defun load-libs ()
  (ql:quickload :clog)
  (ql:quickload :uiop)
  (ql:quickload :alexandria)
  (ql:quickload :sqlite)
  (ql:quickload :local-time)
  (ql:quickload :parse-float)
  (ql:quickload :dexador)
  (ql:quickload :slynk))

(defmacro str+ (&rest rest) `(concatenate 'string ,@rest))
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
       ,@body))





