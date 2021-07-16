(in-package :cl-user)
(defpackage cl-twitter-clone.web
  (:use :cl
        :caveman2
        :cl-twitter-clone.config
        :cl-twitter-clone.view
        :cl-twitter-clone.db
        :websocket-driver)
  (:export :*web*))
(in-package :cl-twitter-clone.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(defvar *connections* (make-hash-table))
(defvar *names* '("Tony Stark"
                  "Steve Rogers"
                  "Natasha Romanoff"
                  "Bruce Banner"
                  "Nick Fury"
                  "Chris Hemsworth"
                  "Tom Hiddleston"))
(defvar *tweets* nil)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html" (list :name (elt *names* (random (length *names*))))))

;; and assign a random nickname to a user upon connection
(defun handle-new-connection (con)
  (setf (gethash con *connections*)
        (format nil "user-~a" (random 100000))))

(defvar *day-names*
           '("Monday" "Tuesday" "Wednesday"
	         "Thursday" "Friday" "Saturday"
	         "Sunday"))

(defun tweet-time ()
  (multiple-value-bind
        (second minute hour day month year day-of-week dst-p tz)
      (get-decoded-time)
    (format t "~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)"
            hour
            minute
            second
            (nth day-of-week *day-names*)
            month
            day
            year
            (- tz))))

(defvar *post* "<div hx-swap-oob='afterbegin:#timeline'>
  <div class='card mb-2 shadow-sm' id='tweet-{{t.id}}'>
    <div class='card-body'>
      <div class='d-flex'>
        <img class='me-4' alt='' src='https://ui-avatars.com/api/?background=random&rounded=true' width='108'/>
        <div>
          <h5 class='card-title text-muted'>
            ~a
            <small> ~a </small>
          </h5>
          <div class='card-text lead mb-2'>
           ~a 
          </div>
        </div>
      </div>
    </div>
  </div>
</div>")

(defun broadcast-tweet (connection message)

  (let ((message (format nil *post*
                         ;; (gethash connection *connections*)
			 (cdr (assoc :username (cl-json:decode-json-from-string message)))
                         (tweet-time)
                         (cdr (assoc :message (cl-json:decode-json-from-string message))))))
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

(defroute "/tweet" ()
  (let ((ws (make-server (lack.request:request-env *request*))))
    (on :open ws
        (lambda () (handle-new-connection ws)))

    (on :message ws
        (lambda (message)
          (format t "~a~%" (assoc :username (cl-json:decode-json-from-string message)))
          ;; construct tweet
          (let ((tweet (list :message (cdr (assoc :message (cl-json:decode-json-from-string message)))
			     :username (cdr (assoc :username (cl-json:decode-json-from-string message)))
                      )))

          (push tweet *tweets*)
          (broadcast-tweet ws message))))

    (lambda (responder)
      (declare (ignore responder))
      (start-connection ws))))

(defroute ("/like/:id" :method :POST) (&key id _parsed)
  )

(defroute ("/retweet/:id" :method :POST) (&key id _parsed)
  )
;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
