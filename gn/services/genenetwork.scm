(define-module (gn services genenetwork))

(use-modules (gnu)
             (gn packages genenetwork)
             (gn packages python)
             (gn packages web))
(use-service-modules web)
(use-package-modules python)

(define %mod-python-path
  (file-append mod-python "/lib/python2.7/site-packages"))

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

  (packages (cons* python-2
                   mod-python
                   python2-qtlreaper
                   python2-htmlgen-gn
                   python2-json-GN1
                   python2-piddle
                   python2-pyx-GN1
                   python2-pyxlwriter
                   python2-svg-GN1
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
