(in-package :stumpwm-dbus)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defvar *active-notifications* nil)
(defvar *notifications-callbacks* nil)
(defvar *notifications-lock* (bordeaux-threads:make-lock "Notifications lock"))

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
  ()
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
  (format stream "~&")
  (clim:with-text-style (stream (clim:make-text-style nil nil 8))
    (format stream "~a" (notification/app-name obj)))
  (format stream "~%")
  (clim:with-text-style (stream (clim:make-text-style nil :bold nil))
    (format stream "~a" (notification/title obj)))
  (format stream "~%~a" (notification/body obj)))

(clim:define-presentation-to-command-translator select-notification
    (notification close-notification notifications-frame)
    (obj)
  (list obj))

(define-notifications-frame-command (close-notification :name "Close notification")
    ((obj 'notification))
  (remove-notification obj))

(defun remove-notification (msg)
  (let ((callbacks (bordeaux-threads:with-lock-held (*notifications-lock*)
                     (setf *active-notifications* (remove msg *active-notifications*))
                     *notifications-callbacks*)))
    (dolist (fn callbacks)
      (funcall fn))))

(defun display-notifications (frame stream)
  (declare (ignore frame))
  (let ((m (bordeaux-threads:with-lock-held (*notifications-lock*)
             (copy-seq *active-notifications*))))
    (dolist (msg m)
      (present-to-stream msg stream))))

(defun refresh-frame (frame)
  (with-call-in-event-handler frame
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'notifications))))

(defun process-incoming-notification (frame msg)
  (with-call-in-event-handler frame
    (push msg (notifications-frame/msg frame))
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'notifications))))

(defun poll-loop ()
  (poll-events (lambda (msg)
                 (let ((callbacks (bordeaux-threads:with-lock-held (*notifications-lock*)
                                    (push msg *active-notifications*)
                                    (copy-seq *notifications-callbacks*))))
                   (dolist (fn callbacks)
                     (funcall fn))))))

(defun start-notifications-thread ()
  (bordeaux-threads:make-thread #'poll-loop :name "Dbus notifications poll thread"))

(defun display-frame ()
  (let* ((frame (clim:make-application-frame 'notifications-frame
                                             :width 200 :height 300
                                             :override-redirect t))
         (callback (lambda ()
                     (refresh-frame frame))))
    (bordeaux-threads:with-lock-held (*notifications-lock*)
      (push callback *notifications-callbacks*))
    (unwind-protect
         (clim:run-frame-top-level frame)
      (bordeaux-threads:with-lock-held (*notifications-lock*)
        (setf *notifications-callbacks* (remove callback *notifications-callbacks*))))))

(defun debug-frame ()
  (let ((thread (start-notifications-thread)))
    (unwind-protect
         (display-frame)
      (bordeaux-threads:destroy-thread thread))))
