(in-package :cl-user)
(defpackage cl-twitter-clone.web
  (:use :cl
        :caveman2
        :cl-twitter-clone.config
        :cl-twitter-clone.view
        :cl-twitter-clone.db
        :websocket-driver
        :cl-who)
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
  "Handle new socket connection, push entry to hash"
  (setf (gethash con *connections*)
        (format nil "user-~a" (random 100000))))

(defvar *avatar-url* "https://ui-avatars.com/api/?background=random&rounded=true&name=")

(defmacro likes-component (tweet)
  `(htm
    (:button
     :class "btn btn-link text-decoration-none"
     :id (concatenate 'string "like-" (write-to-string (getf ,tweet :id)))
     :type "button"
     :hx-post (concatenate 'string "/like/" (write-to-string (getf ,tweet :id)))
     (str (concatenate 'string "Like ( " (write-to-string (getf ,tweet :likes)) " )")))))

(defmacro retweets-component (tweet)
  `(htm
    (:button
     :class "btn btn-link ps-0 text-decoration-none"
     :id (concatenate 'string "retweet-" (write-to-string (getf ,tweet :id)))
     :type "button"
     :hx-post (concatenate 'string "/retweet/" (write-to-string (getf ,tweet :id)))
     (str (concatenate 'string "Retweet ( " (write-to-string (getf ,tweet :retweets)) " )")))))

(defmacro post-component (tweet)
  `(htm
    (:div :hx-swap-oob "afterbegin:#timeline"
          (:div
           :class "card mb-2 shadow-sm"
           :id (concatenate 'string "tweet-" (write-to-string (getf ,tweet :id)))
                (:div :class "card-body"
                      (:div :class "d-flex"
                            (:img :class "me-4" :alt "avatar" :src (concatenate 'string *avatar-url* (getf ,tweet :username)))
                            (:div
                             (:h5 :class "card-title text-muted" (str (getf ,tweet :username))
                                  (:small (str (getf tweet :time))))
                             (:div :class "card-text lead mb-2" (str (getf ,tweet :message)))
                             (likes-component ,tweet)
                             (retweets-component ,tweet))))))))

(defun broadcast-message (message)
  "Broadcast message to all connected clients"
  (loop :for con :being :the :hash-key :of *connections* :do
    (websocket-driver:send con message)))

(defun broadcast-tweet (tweet)
  (let ((message (with-html-output-to-string (*standard-output* nil :indent t)
                   (post-component tweet))))
    (broadcast-message message)))

(defun broadcast-likes (tweet)
  (let ((message (with-html-output-to-string (*standard-output* nil :indent t)
                   (likes-component tweet))))
    (broadcast-message message)))

(defun broadcast-retweets (tweet)
  (let ((message (with-html-output-to-string (*standard-output* nil :indent t)
                   (retweets-component tweet))))
    (broadcast-message message)))

(defroute "/tweet" ()
  (let ((ws (make-server (lack.request:request-env *request*))))
    ;; on socket open, handle new connection
    (on :open ws
        (lambda () (handle-new-connection ws)))

    ;; on socket message, create new tweet and broadcast
    (on :message ws
        (lambda (message)
          (format t "~a~%" (assoc :username (cl-json:decode-json-from-string message)))
          ;; construct tweet
          (let ((tweet (list :message (cdr (assoc :message (cl-json:decode-json-from-string message)))
                             :username (cdr (assoc :username (cl-json:decode-json-from-string message)))
                             :time (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+)
                             :id (get-universal-time)
                             :likes 0
                             :retweets 0)))

            (push tweet *tweets*)
            (broadcast-tweet tweet))))

    ;; otherwise, start ws server
    (lambda (responder)
      (declare (ignore responder))
      (start-connection ws))))

(defun find-tweet (id)
  "Find tweet by id"
  (car (remove-if #'(lambda (twt)
                      (if (string= id (write-to-string (getf twt :id)))
                          nil
                          t)) *tweets*)))

(defroute ("/like/:id" :method :POST) (&key id)
  (let ((tweet (find-tweet id)))
    (incf (getf tweet :likes))
    (broadcast-likes tweet)
    (with-html-output-to-string (*standard-output* nil :indent t)
      (likes-component tweet))))

(defroute ("/retweet/:id" :method :POST) (&key id)
  (let ((tweet (find-tweet id)))
    (incf (getf tweet :retweets))
    (broadcast-retweets tweet)
    (with-html-output-to-string (*standard-output* nil :indent t)
      (retweets-component tweet))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
