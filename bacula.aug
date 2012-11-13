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

   let semicolon = del /([ \t]*;)?/ ""
   let eol = del /[ \t]*(;|(#[ \t]*)?\n)/ "\n"
   let comment_or_eol = Util.comment_eol | eol
   let comment_or_semicolon = Util.comment_eol | semicolon

   let line (sto:lens) = [ sto . comment_or_eol ]
   let line_noeol (sto:lens) = [ sto . comment_or_semicolon ]

   let directive =
        let entry = Util.empty | (indent . (line keyvalue|line include))
     in let entry_noindent = line keyvalue|line include
     in let entry_noindent_noeol = line_noeol keyvalue | line_noeol include
     in let entry_noeol = indent . entry_noindent_noeol
     in [ key /[a-zA-Z]+/
        . Build.block_generic
            entry                      (* entry *)
            entry_noindent             (* entry_noindent *)
            entry_noeol                (* entry_noeol *)
            entry_noindent_noeol       (* entry_noindent_noeol *) 
            Util.comment               (* comment *)
            Util.comment_noindent      (* comment_noindent *)
            Build.block_ldelim_re      (* ldelim_re *)
            Build.block_rdelim_re      (* rdelim_re *) 
            " {\n\t"                   (* ldelim_default *) 
            Build.block_rdelim_default (* rdelim_default *) 
        . Util.eol ]

   let lns = (directive|Util.empty|Util.comment)*

   let filter = incl "/etc/bacula/*.conf"
              . Util.stdexcl

   let xfm = transform lns filter

   test (Bacula.line keyvalue) get "Name = kaki-sd\n" =
      {"Name" = "kaki-sd"}

   test (Bacula.line include) get "@foobar\n" =
      {"@include" = "foobar"}

   test (Bacula.line keyvalue) get "Name = kaki-sd;" =
      {"Name" = "kaki-sd"}

   test (Bacula.line include) get "@foobar  ;" =
      {"@include" = "foobar"}

   test Bacula.lns get "Storage {\n   Name = kaki-sd\n}\n" =
      {"Storage"
         {"Name" = "kaki-sd"}
      }

   (* value can have quotes *)
   test Bacula.lns get "Storage {\n   Name = \"kaki sd\"\n}\n" =
      {"Storage"
         {"Name" = "kaki sd"}
      }

   (* whitespace in key *)
   test Bacula.lns get "Storage {\n   Pid Directory = kaki sd\n}\n" =
      {"Storage"
         {"Pid Directory" = "kaki sd"}
      }

   (* semicolon *)
   test Bacula.lns get "Storage {\n   Name = kaki-sd;\n}\n" =
      {"Storage"
         {"Name" = "kaki-sd" }
      }

   (* inline comment *)
   test Bacula.lns get "Storage {\n   Name = kaki-sd         # just a comment\n}\n" =
      {"Storage"
         {"Name" = "kaki-sd"
           { "#comment" = "just a comment"} }
      }

   (* multiple values *)
   test Bacula.lns get "Storage {\n  Name = kaki sd\nFoo = moo\n}\n" =
      {"Storage"
         {"Name" = "kaki sd"}
         {"Foo" = "moo"}
      }

   (* newline comment *)
   test Bacula.lns get "Storage {\n  Name = kaki sd\n# just a comment\n}\n" =
      {"Storage"
         {"Name" = "kaki sd" }
         {"#comment" = "just a comment" }
      }

   (* TODO: include statements *)
   test Bacula.lns get "Storage {\n  @/etc/foo.conf\n}\n" =
      {"Storage"
         {"@include" = "/etc/foo.conf"}
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

   test Bacula.lns get "Storage {\n   Name = kaki sd}\n" =
      {"Storage"
         {"Name" = "kaki sd"}
      }

   (* TODO: comment at end of line with } *)
