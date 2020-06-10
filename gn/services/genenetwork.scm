(define-module (gn services genenetwork))

(use-modules (gnu)
             (ice-9 match)
             (gn packages genenetwork)
             (gn packages python24)
             (gn packages web))
(use-service-modules web)
(use-package-modules python)

(define %python-path
  "/lib/python2.4/site-packages")

(define %mod-python-path
  (file-append httpd-mod-python-24 %python-path))

(define %default-httpd22-modules
  (map (match-lambda
         ((name file)
          (httpd-module
           (name name)
           (file file))))
       '(("authn_file_module" "modules/mod_authn_file.so")
         ("authn_dbm_module" "modules/mod_authn_dbm.so")
         ("authn_anon_module" "modules/mod_authn_anon.so")
         ("authn_dbd_module" "modules/mod_authn_dbd.so")
         ("authn_default_module" "modules/mod_authn_default.so")
         ("authz_host_module" "modules/mod_authz_host.so")
         ("authz_groupfile_module" "modules/mod_authz_groupfile.so")
         ("authz_user_module" "modules/mod_authz_user.so")
         ("authz_dbm_module" "modules/mod_authz_dbm.so")
         ("authz_owner_module" "modules/mod_authz_owner.so")
         ("authz_default_module" "modules/mod_authz_default.so")
         ("auth_basic_module" "modules/mod_auth_basic.so")
         ("auth_digest_module" "modules/mod_auth_digest.so")
         ("dbd_module" "modules/mod_dbd.so")
         ("dumpio_module" "modules/mod_dumpio.so")
         ("reqtimeout_module" "modules/mod_reqtimeout.so")
         ("ext_filter_module" "modules/mod_ext_filter.so")
         ("include_module" "modules/mod_include.so")
         ("filter_module" "modules/mod_filter.so")
         ("substitute_module" "modules/mod_substitute.so")
         ("log_config_module" "modules/mod_log_config.so")
         ("logio_module" "modules/mod_logio.so")
         ("env_module" "modules/mod_env.so")
         ("mime_magic_module" "modules/mod_mime_magic.so")
         ("expires_module" "modules/mod_expires.so")
         ("headers_module" "modules/mod_headers.so")
         ("ident_module" "modules/mod_ident.so")
         ("setenvif_module" "modules/mod_setenvif.so")
         ("version_module" "modules/mod_version.so")
         ("ssl_module" "modules/mod_ssl.so")
         ("mime_module" "modules/mod_mime.so")
         ("dav_module" "modules/mod_dav.so")
         ("status_module" "modules/mod_status.so")
         ("autoindex_module" "modules/mod_autoindex.so")
         ("asis_module" "modules/mod_asis.so")
         ("info_module" "modules/mod_info.so")
         ("cgi_module" "modules/mod_cgi.so")
         ("dav_fs_module" "modules/mod_dav_fs.so")
         ("vhost_alias_module" "modules/mod_vhost_alias.so")
         ("negotiation_module" "modules/mod_negotiation.so")
         ("dir_module" "modules/mod_dir.so")
         ("imagemap_module" "modules/mod_imagemap.so")
         ("actions_module" "modules/mod_actions.so")
         ("speling_module" "modules/mod_speling.so")
         ("userdir_module" "modules/mod_userdir.so")
         ("alias_module" "modules/mod_alias.so")
         ("rewrite_module" "modules/mod_rewrite.so"))))

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
                   ;httpd-mod-python-24
                   ;python24-qtlreaper
                   ;python24-htmlgen-GN1
                   ;python24-json-GN1
                   ;python24-piddle
                   ;python24-pyx-GN1
                   ;python24-pyxlwriter
                   ;python24-svg-GN1
                   %base-packages))

  (services (list (service httpd-service-type
                           (httpd-configuration
                             (package httpd-mod-python-24) ; Must be this package!
                             (config
                               (httpd-config-file
                                 (server-name "www.genenetwork.org")
                                 (document-root (file-append genenetwork1 "/web"))
                                 (listen '("8042"))
                                 (modules (cons*
                                            (httpd-module
                                              (name "python_module")
                                              (file "modules/mod_python.so"))
                                            %default-httpd-modules))
                                 (extra-config (list "\
TypesConfig etc/httpd/mime.types
PythonPath \"sys.path+['" (file-append python-2.4 "/lib/python2.4") "', '" %mod-python-path "', '" (file-append genenetwork1 "/web/webqtl") "']\"
<Directory " (file-append genenetwork1 "/web/webqtl") ">
  AddHandler mod_python .py
  PythonHandler mod_python.publisher
  PythonAutoReload Off
  PythonDebug On
</Directory>
<Location /mpinfo>
  SetHandler python-program
  PythonHandler mod_python.testhandler
</Location>")))))))))

;PythonPath \"sys.path+['" (file-append python-2.4 "/lib/python2.4") "', '" %mod-python-path "', '" (file-append python24-qtlreaper %python-path) "', '" (file-append python24-json-GN1 %python-path) "', '" (file-append python24-piddle %python-path) "', '" (file-append python24-pyx-GN1 %python-path) "', '" (file-append python24-pyxlwriter-GN1 %python-path) "', '" (file-append python24-svg-GN1 %python-path) "', '" (file-append python24-htmlgen-GN1) "', '" (file-append genenetwork1 "/web/webqtl") "']\"
;PythonPath \"sys.path+['" (file-append python-2.4 "/lib/python2.4") "', '" %mod-python-path "', '" (file-append genenetwork1 "/web/webqtl") "']\"
