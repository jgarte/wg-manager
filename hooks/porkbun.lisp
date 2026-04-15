#|
exec $WG_MANAGER run "$0" "$@"
|#

(defpackage #:wg-manager.porkbun
  (:use #:cl))

(in-package #:wg-manager.porkbun)

(defvar *key*)
(defvar *secret*)
(defvar *domain*)
(defvar *suffix*)

(defun call (method endpoint &optional payload)
  (let ((data (com.inuoe.jzon:parse
               (drakma:http-request
                (format NIL "https://api.porkbun.com/api/json/v3/~a" endpoint)
                :method method
                :content-type "application/json"
                :content (com.inuoe.jzon:stringify (alexandria:alist-hash-table
                                                    `(("apikey" . ,*key*)
                                                      ("secretapikey" . ,*secret*)
                                                      ,@payload)))
                :want-stream T))))
    (if (string= "SUCCESS" (gethash "status" data))
        data
        (error "API call failed!~%~%~a" (com.inuoe.jzon:stringify data :pretty T)))))

(defun find-record (name)
  (let ((name (format NIL "~a.~a" name *domain*)))
    (loop for record across (gethash "records" (call :get (format NIL "dns/retrieve/~a" *domain*)))
          do (when (string-equal name (gethash "name" record))
               (return (gethash "id" record))))))

(defun sanitize (name)
  (with-output-to-string (out)
    (loop for c across name
          do (cond ((or (alphanumericp c) (char= #\- c))
                    (write-char c out))
                   ((char= #\/ c)
                    (write-char #\- out))))))

(defun main ()
  (destructuring-bind (self change ipv4 name &optional public-key note) sb-ext:*posix-argv*
    (declare (ignore self public-key))
    (setf *key* (or (uiop:getenv "WG_PORKBUN_KEY") ""))
    (setf *secret* (or (uiop:getenv "WG_PORKBUN_SECRET") ""))
    (setf *domain* (or (uiop:getenv "WG_PORKBUN_DOMAIN") ""))
    (setf *suffix* (uiop:getenv "WG_PORKBUN_SUFFIX"))
    (if (or (string= "" *key*) (string= "" *secret*) (string= "" *domain*))
        (format *error-output* "~& Skipping Porkbun hook: WG_PORKBUN_KEY or WG_PORKBUN_SECRET unset.~%")
        (let* ((name (format NIL "~a~@[.~a~]" (sanitize name) *suffix*))
               (data `(("name" . ,name)
                       ("type" . "A")
                       ("content" . ,ipv4)
                       ("comment" . ,(or note 'null))
                       ("ttl" . 1)))
               (existing (find-record name)))
          (cond ((string-equal change "add")
                 (unless existing
                   (call :post (format NIL "dns/create/~a" *domain*) data)))
                ((string-equal change "remove")
                 (when existing
                   (call :delete (format NIL "dns/delete/~a/~a" *domain* existing))))
                ((string-equal change "edit")
                 (when existing
                   (call :delete (format NIL "dns/delete/~a/~a" *domain* existing)))
                 (call :post (format NIL "dns/create/~a" *domain*) data)))))))

(main)
