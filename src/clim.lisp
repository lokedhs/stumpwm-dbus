(in-package :stumpwm-dbus)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(defun call-in-event-handler (frame fn)
  (clim:execute-frame-command frame `(funcall ,(lambda () (funcall fn))))
  nil)

(defmacro with-call-in-event-handler (frame &body body)
  `(call-in-event-handler ,frame (lambda () ,@body)))

(defclass notifications-view (clim:view)
  ())

(clim:define-application-frame notifications-frame ()
  ((notifications-list :type list
                       :initform nil
                       :accessor notifications-frame/msg))
  (:panes (notifications :application
                         :default-view (make-instance 'notifications-view)
                         :display-function 'display-notifications
                         :scroll-bars :vertical))
  (:layouts (default (clim:vertically ()
                       notifications))))

(defmethod clim-internals::frame-is-override-redirect-p ((frame notifications-frame))
  t)

(defmethod clim:frame-standard-output ((frame notifications-frame))
  (clim:find-pane-named frame 'notifications))

(clim:define-presentation-method clim:present (obj (type notification) stream (view notifications-view) &key)
  (format stream "~a: ~a~%~a"
          (notification/app-name obj)
          (notification/title obj)
          (notification/body obj)))

(defun display-notifications (frame stream)
  (dolist (msg (notifications-frame/msg frame))
    (present-to-stream msg stream)))

(defun process-incoming-notification (frame msg)
  (with-call-in-event-handler frame
    (push msg (notifications-frame/msg frame))
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'notifications))))

(defun make-notifications-thread (fn)
  (bordeaux-threads:make-thread (lambda ()
                                  (poll-events fn))
                                :name "Dbus notifications poll thread"))

(defun display-frame ()
  (let* ((frame (clim:make-application-frame 'notifications-frame
                                             :width 200 :height 300
                                             :override-redirect t))
         (poll-thread (make-notifications-thread (lambda (msg) (process-incoming-notification frame msg)))))
    (unwind-protect
         (clim:run-frame-top-level frame)
      (bordeaux-threads:destroy-thread poll-thread))))
