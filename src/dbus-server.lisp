(in-package :stumpwm-dbus)

(defvar *current-id* 1)
(defvar *current-id-lock* (bordeaux-threads:make-lock))

(defclass notification ()
  ((id             :type integer
                   :reader notification/id)
   (title          :type string
                   :initarg :title
                   :reader notification/title)
   (body           :type string
                   :initarg :body
                   :reader notification/body)
   (app-name       :type string
                   :initarg :app-name
                   :reader notification/app-name)
   (actions        :initarg :actions
                   :reader notification/actions)
   (expire-timeout :type integer
                   :initarg :expire-timeout
                   :reader notification/expire-timeout)
   (created-date   :type integer
                   :initarg :created-date
                   :reader notification/created-date)))

(defmethod initialize-instance :after ((obj notification) &key)
  (setf (slot-value obj 'id)
        (bordeaux-threads:with-lock-held (*current-id-lock*)
          (prog1
              *current-id*
            (incf *current-id*)))))

(defmethod print-object ((obj notification) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (id title app-name) obj
      (format stream "~a ~s ~s" id title app-name))))

(dbus:define-dbus-object notification-service
  (:path "/org/freedesktop/Notifications"))

(dbus:define-dbus-method (notification-service get-server-information) () (:string :string :string :string)
  (:interface "org.freedesktop.Notifications")
  (values "StumpWM notifications service" "com.dhsdevelopments" "1.0" "1.2"))

(dbus:define-dbus-method (notification-service get-capabilities) () (:array :string)
  (:interface "org.freedesktop.Notifications")
  (format t "get-capabilities~%")
  nil)

(defvar *notification-callback* nil
  "Bound to the callback function during the evaluation of POLL-EVENTS")

;;     (:in "susssasa{sv}i" :out "i" :dbus-name "Notify")
(dbus:define-dbus-method (notification-service notify)
    ((app-name :string)
     (replaces-id :uint32)
     (app-icon :string)
     (summary :string)
     (body :string)
     (actions (:array :string))
     (hints (:array (:dict-entry :string :variant)))
     (expire-timeout :int32))
    (:int32)
  (:interface "org.freedesktop.Notifications")

  (declare (ignore app-icon hints replaces-id))

  (let ((notification (make-instance 'notification
                                     :title summary
                                     :body body
                                     :app-name app-name
                                     :actions actions
                                     :expire-timeout expire-timeout
                                     :created-date (get-universal-time))))
    (funcall *notification-callback* notification)
    (notification/id notification)))

(defun poll-events (notification-received-fn)
  (let ((*notification-callback* notification-received-fn))
    (dbus:with-open-bus (bus (dbus:session-server-addresses))
      (dbus:request-name bus "org.freedesktop.Notifications")
      (dbus:publish-objects (dbus:bus-connection bus)))))
