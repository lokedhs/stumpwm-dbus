(in-package :stumpwm-dbus)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defvar *active-notifications* nil)
(defvar *current-frame* nil)
(defvar *notifications-lock* (bordeaux-threads:make-lock "Notifications lock"))

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(defun call-in-event-handler (frame fn)
  (clim:execute-frame-command frame `(funcall ,(lambda () (funcall fn))))
  nil)

(defmacro with-call-in-event-handler (frame &body body)
  `(call-in-event-handler ,frame (lambda () ,@body)))

(defvar *debug* nil)

(defun run-at-time (fn time)
  (if *debug*
      (progn
        #+sbcl (sb-ext:schedule-timer (sb-ext:make-timer fn) time)
        #-sbcl (error "Debug mode only works on SBCL"))
      (stumpwm:run-with-timer time nil fn)))

;;;
;;;  popup
;;;

(defclass popup-view (clim:view)
  ()
  (:documentation "View that is used to display messages in a popup frame."))

(defgeneric popup-timeout (msg)
  (:method (msg) 5)
  (:documentation "The time in seconds that the given message will be displayed.
If NIL, the popup has to be closed manually. If :NONE, the
popup will not be displayed at all."))

(clim:define-application-frame popup-frame ()
  ((notification :initarg :notification
                 :reader popup-frame/notification))
  (:panes (content :application
                   :default-view (make-instance 'popup-view)
                   :display-function 'display-popup-content
                   :scroll-bars :vertical))
  (:layouts (default (clim:vertically ()
                       content))))

(defmethod clim-extensions:find-frame-type ((frame popup-frame))
  :override-redirect)

(defun display-popup-content (frame stream)
  (present-to-stream (popup-frame/notification frame) stream))

(defmethod clim:note-frame-enabled :after (fn (frame popup-frame))
  nil)

(defun open-popup (msg)
  (let ((timeout (popup-timeout msg)))
    (unless (eq timeout :none)
      (let ((frame (clim:make-application-frame 'popup-frame
                                                :width 200 :height 100
                                                :notification msg)))
        (when timeout
          (run-at-time (lambda ()
                         (with-call-in-event-handler frame
                           (clim:frame-exit frame)))
                       timeout))
        (clim:run-frame-top-level frame)))))

;;;
;;;  notifications-frame
;;;

(defclass notifications-view (clim:view)
  ())

(clim:define-application-frame notifications-frame ()
  ()
  (:panes (notifications :application
                         :default-view (make-instance 'notifications-view)
                         :display-function 'display-notifications
                         :scroll-bars :vertical))
  (:layouts (default (clim:vertically ()
                       notifications))))

(defmethod clim-extensions:find-frame-type ((frame notifications-frame))
  :dialog)

(defmethod clim:frame-standard-output ((frame notifications-frame))
  (clim:find-pane-named frame 'notifications))

(clim:define-presentation-to-command-translator select-notification
    (notification close-notification notifications-frame)
    (obj)
  (list obj))

(define-notifications-frame-command (close-notification :name "Close notification")
    ((obj 'notification))
  (remove-notification obj))

(define-notifications-frame-command (close-all-notifications :name "Close all notifications" :keystroke (#\c))
    ()
  (remove-all))

(define-notifications-frame-command (close-frame :name "Close frame" :keystroke (:escape))
    ()
  (clim:frame-exit clim:*application-frame*))

(defmethod clim:handle-event :after (frame event)
  (log:trace "Got event: ~s, on port: ~s" event frame))

(defun remove-notification (msg)
  (bordeaux-threads:with-lock-held (*notifications-lock*)
    (setf *active-notifications* (remove msg *active-notifications*))))

(defun remove-all ()
  (bordeaux-threads:with-lock-held (*notifications-lock*)
    (setf *active-notifications* nil)))

(defun display-notifications (frame stream)
  (declare (ignore frame))
  (let ((m (bordeaux-threads:with-lock-held (*notifications-lock*)
             (copy-seq *active-notifications*))))
    (dolist (msg (reverse m))
      (present-to-stream msg stream))))

(defun refresh-frame ()
  (let ((frame (bordeaux-threads:with-lock-held (*notifications-lock*)
                 *current-frame*)))
    (when frame
      (with-call-in-event-handler frame
        (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'notifications))))))

(defun process-incoming-message (msg)
  (bordeaux-threads:with-lock-held (*notifications-lock*)
    (push msg *active-notifications*))
  (bordeaux-threads:make-thread (lambda ()
                                  (open-popup msg)))
  (refresh-frame))

(defun notifications-poll-loop ()
  (poll-events #'process-incoming-message))

(defun pidgin-messages-poll-loop ()
  (pidgin-listener #'process-incoming-message))

(defun start-notifications-thread ()
  (bordeaux-threads:make-thread #'notifications-poll-loop :name "Dbus notifications poll thread")
  (bordeaux-threads:make-thread #'pidgin-messages-poll-loop :name "Dbus pidgin messages poll thread"))

(defun display-frame ()
  (let* ((frame (clim:make-application-frame 'notifications-frame
                                             :width 200 :height 300
                                             :override-redirect t)))
    (bordeaux-threads:with-lock-held (*notifications-lock*)
      (setq *current-frame* frame))
    (unwind-protect
         (clim:run-frame-top-level frame)
      (bordeaux-threads:with-lock-held (*notifications-lock*)
        (setq *current-frame* nil)))))

(defun debug-frame ()
  (let ((thread (start-notifications-thread)))
    (unwind-protect
         (display-frame)
      (bordeaux-threads:destroy-thread thread))))

;;;
;;;  dbus notifications
;;;

(clim:define-presentation-method clim:present (obj (type notification) stream view &key)
  (format stream "~&")
  (clim:with-text-style (stream (clim:make-text-style nil nil 8))
    (format stream "~a" (notification/app-name obj)))
  (format stream "~%")
  (clim:with-text-style (stream (clim:make-text-style nil :bold nil))
    (format stream "~a" (notification/title obj)))
  (format stream "~%~a~%~%" (notification/body obj)))

(defmethod popup-timeout ((msg notification))
  5)

;;;
;;;  pidgin messages
;;;

(clim:define-presentation-method clim:present (obj (type pidgin-message) stream view &key)
  (format stream "~&")
  (clim:with-text-style (stream (clim:make-text-style nil :bold nil))
    (format stream "Message from: ~a" (pidgin-message/sender-id obj)))
  (format stream "~%")
  (present-html stream (pidgin-message/content obj)))

(defmethod popup-timeout ((msg pidgin-message))
  10)

