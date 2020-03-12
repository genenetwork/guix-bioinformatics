(define-module (gn services genenetwork))

(use-modules (gnu)
             (gn packages genenetwork)
             (gn packages python24)
             (gn packages web))
(use-service-modules web)
(use-package-modules python)

(define %mod-python-path
  (file-append mod-python "/lib/python2.4/site-packages"))

(operating-system
  (host-name "genenetwork")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "does-not-matter")))
  (file-systems %base-file-systems)
  ;; No firmware for VMs
  (firmware '())

  (packages (cons* python-2.4
                   mod-python
                   python24-qtlreaper
                   ;python24-htmlgen-gn
                   python24-json-GN1
                   python24-piddle
                   python24-pyx-GN1
                   python24-pyxlwriter
                   python24-svg-GN1
                   %base-packages))

  (services (list (service httpd-service-type
                           (httpd-configuration
                             (config
                               (httpd-config-file
                                 (server-name "www.genenetwork.org")
                                 (document-root (file-append genenetwork1 "/web"))
                                 (listen '("8811"))
                                 (modules (cons*
                                            (httpd-module
                                              (name "python_module")
                                              (file (file-append mod-python "/modules/mod_python.so")))
                                            %default-httpd-modules))
                                 (extra-config (list "\
PythonPath \"sys.path+['" %mod-python-path "', '" (file-append genenetwork1 "/web/webqtl") "']\"
<Directory " (file-append genenetwork1 "/web/webqtl") ">
  SetHandler python-program
  PythonHandler mod_python.publisher
  PythonAutoReload Off
</Directory>
<Location /mpinfo>
  SetHandler python-program
  PythonHandler mod_python.testhandler
</Location>")))))))))
