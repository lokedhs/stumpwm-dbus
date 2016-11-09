(asdf:defsystem #:stumpwm-dbus
  :description "Dbus integration for StumpWM"
  :license "Apache"
  :serial t
  :depends-on (:stumpwm
               :mcclim
               :bordeaux-threads
               :alexandria
               :dbus
               :log4cl)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "dbus-server")
                                     (:file "clim")
                                     (:file "commands")))))
