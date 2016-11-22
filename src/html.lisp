(in-package :stumpwm-dbus)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *html-namespace* "http://www.w3.org/1999/xhtml")

(defun iterate-node (stream node)
  (labels ((iterate-child-nodes ()
             (loop
               for child-node across (dom:child-nodes node)
               do (iterate-node stream child-node))))
    (if (dom:text-node-p node)
        (format stream "~a" (dom:node-value node))
        ;; It's not a text node, so process its content
        (let ((name (dom:node-name node)))
          (cond ((equal name "b")
                 (clim:with-text-style (stream (clim:make-text-style nil :bold nil))
                   (iterate-child-nodes)))
                ((equal name "i")
                 (clim:with-text-style (stream (clim:make-text-style nil :italic nil))
                   (iterate-child-nodes)))
                (t
                 (iterate-child-nodes)))))))

(defun present-html (stream text)
  (let ((doc (closure-html:parse text (cxml-dom:make-dom-builder))))
    (iterate-node stream doc)))
