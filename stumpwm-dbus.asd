(asdf:defsystem #:stumpwm-dbus
  :description "Dbus integration for StumpWM"
  :license "Apache"
  :serial t
  :depends-on (:stumpwm
               :mcclim
               :bordeaux-threads
               :alexandria
               :dbus
               :log4cl
               :cxml
               :cxml-dom
               :closure-html)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "html")
                                     (:file "dbus-server")
                                     (:file "pidgin")
                                     (:file "commands")
                                     (:file "media")
                                     (:file "clim")))))
