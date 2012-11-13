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

   let indent = Util.del_opt_ws "\t"

   let equal = del /[ \t]*=[ \t]*/ " = "
   let key_name = /[a-zA-Z][a-zA-Z ]+[a-zA-Z]/
   let dquote = del /"?/ "\""

   let val = dquote . store /[^"#\n\t; ][^"#\n;]*[^"#\n\t; ]/ . dquote

   let keyvalue = key key_name . equal . val
   let include = label "@include" . del "@" "@" . store /[^ #\t\n@};]+/

   let semicolon = [ del /[ \t]*;/ ";" ]
   let line (sto:lens) = [ indent . sto . (semicolon|Util.comment_or_eol) ]
   let brackets = del /[ \n\t]*\{\n*/ " {\n" . (line keyvalue |line include)+ . del /[ \t\n]*}/ "\n}"

   let directive = [ key /[a-zA-Z]+/ . brackets ]

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

   (* semicolon *)
   test Bacula.lns get "Storage {\n   Name = kaki-sd;\n}" =
      {"Storage"
         {"Name" = "kaki-sd" {} }
      }

   (* inline comment *)
   test Bacula.lns get "Storage {\n   Name = kaki-sd         # just a comment\n}" =
      {"Storage"
         {"Name" = "kaki-sd"
           { "#comment" = "just a comment"} }
      }

   (* multiple values *)
   test Bacula.lns get "Storage {\n  Name = kaki sd\nFoo = moo\n}" =
      {"Storage"
         {"Name" = "kaki sd"}
         {"Foo" = "moo"}
      }

   (* newline comment *)
   test Bacula.lns get "Storage {\n  Name = kaki sd\n# just a comment\n}" =
      {"Storage"
         {"Name" = "kaki sd"
           {"#comment" = "just a comment"}
           {} }
      }

   (* TODO: include statements *)
   test Bacula.lns get "Storage {\n  @/etc/foo.conf\n}" =
      {"Storage"
         {"@include" = "/etc/foo.conf"}
         {}
      }
   
   (* TODO: support nested directives
   test Bacula.lns get "FileSet {
  Name = \"DefaultSet\"
  Include {
    Options {
      signature = SHA1
      noatime = yes
    }
    File = /etc
  }
}" =
      {"FileSet"
         {"Name" = "DefaultSet"}
         {"Include"
            {"Options"
              {"signature" = "SHA1"}
              {"noatime" = "yes"}
            }
            {"File" = "/etc"}
         }
      }
*)

   (* TODO: no endline
   test Bacula.lns get "Storage {\n   Name = kaki sd}" =
      {"Storage"
         {"Name" = "kaki sd"}
      }
   *)

   (* TODO: comment at end of line with } *)
