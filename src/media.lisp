(in-package :stumpwm-dbus)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun prefix-match (prefix name)
  (let ((prefix-length (length prefix)))
    (and (>= (length name) prefix-length)
         (string= prefix name :end2 prefix-length))))

(defun find-media-providers (bus)
  (remove-if-not (lambda (name)
                   (prefix-match "org.mpris.MediaPlayer2" name))
                 (dbus:list-names bus)))

(defun inner-call-media-function (bus prefix command &rest args)
  (let ((media-providers (find-media-providers bus)))
    (when media-providers
      (let ((provider (car media-providers)))
        (dbus:with-introspected-object (n bus "/org/mpris/MediaPlayer2" provider)
          (apply #'n prefix command args))))))

(defun call-media-function (prefix command &rest args)
  (dbus:with-open-bus (bus (dbus:session-server-addresses))
    (apply #'inner-call-media-function bus prefix command args)))

(stumpwm:defcommand media-player-play-pause () ()
  "Play or pause the media player"
  (call-media-function "org.mpris.MediaPlayer2.Player" "PlayPause"))

(stumpwm:defcommand media-player-play () ()
  "Play or pause the media player"
  (call-media-function "org.mpris.MediaPlayer2.Player" "Play"))

(stumpwm:defcommand media-player-pause () ()
  "Play or pause the media player"
  (call-media-function "org.mpris.MediaPlayer2.Player" "Pause"))

(stumpwm:defcommand media-player-next () ()
  "Select the next track on the media player"
  (call-media-function "org.mpris.MediaPlayer2.Player" "Next"))

(stumpwm:defcommand media-player-previous () ()
  "Select the previous track on the media player"
  (call-media-function "org.mpris.MediaPlayer2.Player" "Previous"))

(stumpwm:defcommand media-player-seek (offset) ((:number "Amount: "))
  "Seek by some given amount"
  (call-media-function "org.mpris.MediaPlayer2.Player" "Seek" (or offset 10)))
