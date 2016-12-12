(defpackage :stumpwm-dbus
  (:use :cl)
  (:documentation "Dbus integration for StumpWM")
  (:export
   #:start-notifications-thread
   #:num-active-notifications
   #:record-notification
   #:notification
   #:notification/id
   #:notification/title
   #:notification/body
   #:notification/app-name
   #:notification/actions
   #:notification/expire-timeout
   #:notification/created-date))
