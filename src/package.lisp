(defpackage :stumpwm-dbus
  (:use :cl)
  (:documentation "Dbus integration for StumpWM")
  (:export
   #:start-notifications-thread
   #:num-active-notifications))
