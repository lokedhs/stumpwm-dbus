(in-package :stumpwm-dbus)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(stumpwm:add-screen-mode-line-formatter #\l 'fmt-num-notifications)

(stumpwm:defcommand open-notifications () ()
  "Open the notifications panel"
  (when (bordeaux-threads:with-lock-held (*notifications-lock*)
          *current-frame*)
    (error "Notifications frame is already open"))
  (bordeaux-threads:make-thread #'display-frame :name "Notifications frame main loop"))

(defun num-active-notifications ()
  (bordeaux-threads:with-lock-held (*notifications-lock*)
    (length *active-notifications*)))

(defun fmt-num-notifications (ml &rest args)
  (declare (ignore ml args))
  (let ((n (bordeaux-threads:with-lock-held (*notifications-lock*)
             (length *active-notifications*))))
    (format nil "~:[none~;~:*~a~]" n)))
