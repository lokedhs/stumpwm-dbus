(in-package :stumpwm-dbus)

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

(defmethod clim:frame-standard-output ((frame notifications-frame))
  (clim:find-pane-named frame 'notifications))

(defun display-notifications (frame stream)
  (declare (ignore frame))
  (format stream "Notifications will be displayed here"))

(defun display-frame ()
  (let ((frame (clim:make-application-frame 'notifications-frame
                                            :width 200 :height 300)))
    (clim:run-frame-top-level frame)))
