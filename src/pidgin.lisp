(in-package :stumpwm-dbus)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(dbus:define-dbus-object pidgin-service
  (:path "/com/dhsdevelopments/PidginService"))

(dbus:define-dbus-signal-handler (pidgin-service quitting) ()
  (:interface "im.pidgin.purple.PurpleInterface")
  (log:info "Got quit message"))

(dbus:define-dbus-signal-handler (pidgin-service account-connecting) ((code :int32))
  (:interface "im.pidgin.purple.PurpleInterface")
  (log:info "Account connecting: ~s" code))

(dbus:define-dbus-method (pidgin-service foo) () ()
  (:interface "com.dhsdevelopments.PidginService")
  (log:info "Foo method called"))

(defun pidgin-listener ()
  (dbus:with-open-bus (bus (dbus:session-server-addresses))
    (dbus:request-name bus "com.dhsdevelopments.PidginService")
    (dbus:publish-objects (dbus:bus-connection bus) '(pidgin-service))))
