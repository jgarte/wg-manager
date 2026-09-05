#|
exec $WG_MANAGER run "$0" "$@"
|#

(defpackage #:wg-manager.inwx
  (:use #:cl))

(in-package #:wg-manager.inwx)

(defvar *cookies* (make-instance 'drakma:cookie-jar))

(defun call (endpoint &optional payload)
  (let ((data (com.inuoe.jzon:parse
               (drakma:http-request
                (format NIL "https://api.domrobot.com/jsonrpc/" endpoint)
                :method :post
                :content-type "application/json"
                :content (com.inuoe.jzon:stringify
                          (alexandria:alist-hash-table
                           `(("method" . ,method)
                             ("params" . ,(alexandria:alist-hash-table payload)))))
                :cookie-jar *cookies*
                :want-stream T))))
    (if (string= "1000" (gethash "code" data))
        (gethash "resData" data)
        (error "API call failed!~%~%~a" (com.inuoe.jzon:stringify data :pretty T)))))

(defun login (user pass)
  (call "account.login" `(("user" . ,user) ("pass" . ,pass))))

(defun find-record (name domain)
  (ignore-errors
   (gethash (aref (gethash (call "nameserver.info" `(("domain" . ,domain) ("name" . ,name))) "record") 0) "id")))

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
    (let ((user (or (uiop:getenv "WG_INWX_USER") ""))
          (pass (or (uiop:getenv "WG_INWX_PASS") ""))
          (domain (or (uiop:getenv "WG_INWX_DOMAIN") ""))
          (suffix (uiop:getenv "WG_INWX_SUFFIX")))
      (if (or (string= "" user) (string= "" pass) (string= "" domain))
          (format *error-output* "~& Skipping INWX hook: WG_INWX_USER or WG_INWX_PASS unset.~%")
          (let* ((name (format NIL "~a~@[.~a~]" (sanitize name) suffix))
                 (data `(("domain" . ,domain)
                         ("name" . ,name)
                         ("type" . "A")
                         ("content" . ,ipv4)
                         ("comment" . ,(or note 'null))
                         ("ttl" . 1)))
                 (existing (find-record name domain)))
            (cond ((string-equal change "add")
                   (unless existing
                     (format *error-output* "~&Adding record to INWX.~%")
                     (call "nameserver.createRecord" data)))
                  ((string-equal change "remove")
                   (when existing
                     (format *error-output* "~&Removing record from INWX.~%")
                     (call "nameserver.deleteRecord" `(("id" . ,existing)))))
                  ((string-equal change "edit")
                   (format *error-output* "~&Updating record in INWX.~%")
                   (if existing
                       (call "nameserver.updateRecord" (list* `("id" . ,existing) data))
                       (call "nameserver.createRecord" data)))))))))

(main)
