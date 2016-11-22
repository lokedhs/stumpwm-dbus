(in-package :stumpwm-dbus)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defclass pidgin-message ()
  ((message-id :initarg :message-id
               :reader pidgin-message/message-id)
   (sender-id  :initarg :sender-id
               :reader pidgin-message/sender-id)
   (content    :initarg :content
               :reader pidgin-message/content)))

(dbus:define-dbus-object pidgin-service
  (:path "/im/pidgin/purple/PurpleObject"))

(dbus:define-dbus-signal-handler (pidgin-service quitting) ()
  (:interface "im.pidgin.purple.PurpleInterface")
  (log:info "Got quit message"))

(dbus:define-dbus-signal-handler (pidgin-service account-connecting)
    ((code :int32))
  (:interface "im.pidgin.purple.PurpleInterface")
  (log:info "Account connecting: ~s" code))

(dbus:define-dbus-signal-handler (pidgin-service received-im-msg)
    ((message-id :int32)
     (sender-id :string)
     (content :string)
     (unknown0 :int32)
     (unknown1 :uint32))
  (:interface "im.pidgin.purple.PurpleInterface")
  (log:debug "Got message from ~s: ~s (~s, ~s, ~s)" sender-id content message-id unknown0 unknown1)
  (let ((msg (make-instance 'pidgin-message :message-id message-id :sender-id sender-id :content content)))
    (funcall *notification-callback* msg))
  nil)

(dbus:define-dbus-method (pidgin-service foo) () ()
  (:interface "com.dhsdevelopments.PidginService")
  (log:info "Foo method called"))

(defun pidgin-listener (callback)
  (let ((*notification-callback* callback))
    (dbus:with-open-bus (bus (dbus:session-server-addresses))
      (dbus:add-match bus :type :signal :interface "im.pidgin.purple.PurpleInterface")
      (dbus:request-name bus "com.dhsdevelopments.PidginService")
      (dbus:publish-objects (dbus:bus-connection bus) '(pidgin-service)))))
