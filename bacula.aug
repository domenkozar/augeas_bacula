(*
Module: Bacula
  Parses: /etc/bacula/*.conf

Author: Domen Kožar <domen@dev.si>

About: Reference
  This lens tries to be ...

About: License
  This file is licenced under the LGPL v2+, like the rest of Augeas.

About: Lens Usage
   See <lns>.

About: Configuration files
   This lens applies to /etc/bacula/*.conf.

About: Examples
   The <test_bacula.aug> file contains various examples and tests.
*)


module Bacula =
   autoload xfm

   let val = del /"?/ "\"" . store /[a-zA-Z0-9]/ . del /"?;?/ "\""

   (* TODO: support whitespace in key name *)
   let line = [ Util.del_ws_tab . key /[a-zA-Z0-9]+/ . del /[ \t]*=[ \t*]/ " = " . val . Util.comment_or_eol ]

   (* TODO: support file includes *)
   let content = del /[ \t]*\{/ " {" . Util.comment_or_eol . line . del /[ \t]*\}/ "}"

   (* TODO: support nested directives *)
   let directive = [ key /[a-zA-Z]+/ . content ]

   let lns = (directive|Util.empty|Util.comment)*

   let filter = incl "/etc/bacula/*.conf"
              . Util.stdexcl

   let xfm = transform lns filter

   test Bacula.lns get "Storage {\n   Name = kaki-sd\n}" =
      {"Storage"
         {"Name" = "kaki-sd"}
      }
