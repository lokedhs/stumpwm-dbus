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

;;;
;;;  popup
;;;

(defclass popup-view (clim:view)
  ())

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
  (let ((frame (clim:make-application-frame 'popup-frame
                                            :width 200 :height 100
                                            :notification msg)))
    (stumpwm:call-in-main-thread
     (lambda ()
       (stumpwm:run-with-timer 5 nil (lambda ()
                                       (with-call-in-event-handler frame
                                         (clim:frame-exit frame))))))
    (clim:run-frame-top-level frame)))

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

(clim:define-presentation-method clim:present (obj (type notification) stream view &key)
  (format stream "~&")
  (clim:with-text-style (stream (clim:make-text-style nil nil 8))
    (format stream "~a" (notification/app-name obj)))
  (format stream "~%")
  (clim:with-text-style (stream (clim:make-text-style nil :bold nil))
    (format stream "~a" (notification/title obj)))
  (format stream "~%~a~%~%" (notification/body obj)))

(clim:define-presentation-to-command-translator select-notification
    (notification close-notification notifications-frame)
    (obj)
  (list obj))

(define-notifications-frame-command (close-notification :name "Close notification")
    ((obj 'notification))
  (remove-notification obj))

(define-notifications-frame-command (close-frame :name "Close frame" :keystroke (:b :control))
    ()
  (log:info "Close!"))

(defmethod clim:handle-event :after (frame event)
  (log:trace "Got event: ~s, on port: ~s" event frame))

(defun remove-notification (msg)
  (bordeaux-threads:with-lock-held (*notifications-lock*)
    (setf *active-notifications* (remove msg *active-notifications*))))

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

(defun poll-loop ()
  (poll-events (lambda (msg)
                 (bordeaux-threads:with-lock-held (*notifications-lock*)
                   (push msg *active-notifications*))
                 (bordeaux-threads:make-thread (lambda ()
                                                 (open-popup msg)))
                 (refresh-frame))))

(defun start-notifications-thread ()
  (bordeaux-threads:make-thread #'poll-loop :name "Dbus notifications poll thread"))

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
