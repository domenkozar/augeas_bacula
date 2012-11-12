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

   let val = del /"?/ "\"" . store /[^# \t\n"]([^#\n"]*[^;# \t\n"])?/ . del /"?;?/ "\""

   let indent = Util.del_opt_ws "\t"
   let equal = del /[ \t]*=[ \t]*/ " = "
   let name = /[a-zA-Z][a-zA-Z ]+[a-zA-Z]/

   let line = indent . Build.key_value_line_comment name equal val Util.comment_eol

   (* TODO: support file includes *)
   let content = del /[ \t]*\{/ " {" . Util.comment_or_eol . (line)+ . del /[ \t]*\}/ "}"

   (* TODO: support nested directives *)
   let directive = [ key /[a-zA-Z]+/ . content ]

   let lns = (directive|Util.empty|Util.comment)*

   let filter = incl "/etc/bacula/*.conf"
              . Util.stdexcl

   let xfm = transform lns filter

   (* basic directive *)
   test Bacula.lns get "Storage {\n   Name = kaki-sd\n}" =
      {"Storage"
         {"Name" = "kaki-sd"}
      }

   (* value can have quotes *)
   test Bacula.lns get "Storage {\n   Name = \"kaki sd\"\n}" =
      {"Storage"
         {"Name" = "kaki sd"}
      }

   (* whitespace in key *)
   test Bacula.lns get "Storage {\n   Pid Directory = kaki sd\n}" =
      {"Storage"
         {"Pid Directory" = "kaki sd"}
      }

   (* no endline *)
   test Bacula.lns get "Storage {\n   Name = kaki sd}" =
      {"Storage"
         {"Name" = "kaki sd"}
      }

   (* semicolon *)
   test Bacula.lns get "Storage {\n   Name = kaki-sd;\n}" =
      {"Storage"
         {"Name" = "kaki-sd"}
      }

   (* inline comment *)
   test Bacula.lns get "Storage {\n   Name = kaki-sd         # just a comment\n}" =
      {"Storage"
         {"Name" = "kaki-sd"
           { "#comment" = "just a comment"}
         }
      }

   (* multiple values *)
   test Bacula.lns get "Storage {\n  Name = kaki sd\nFoo = moo\n}" =
      {"Storage"
         {"Name" = "kaki sd"}
         {"Foo" = "moo"}
      }
