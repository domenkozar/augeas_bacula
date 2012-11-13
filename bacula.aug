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

   let val = dquote . store /[^}"#\n\t; ][^}"#\n;]*[^}"#\n\t; ]/ . dquote

   let keyvalue = key key_name . equal . val
   let include = label "@include" . del "@" "@" . store /[^ #\t\n@};]+/

   let semicolon = del /([ \t]*;)?/ ""
   let eol = del /[ \t]*(;|(#[ \t]*)?\n)/ "\n"
   let comment_or_eol = Util.comment_eol | eol
   let comment_or_semicolon = Util.comment_eol | semicolon

   let line (sto:lens) = [ sto . comment_or_eol ]
   let line_noeol (sto:lens) = [ sto . comment_or_semicolon ]

   let rec block =
        let entry = Util.empty | (indent . (line keyvalue | line include | block))
     in let entry_noindent = line keyvalue | line include | block
     in let entry_noindent_noeol = line_noeol keyvalue | line_noeol include | block
     in let entry_noeol = indent . entry_noindent_noeol
     in [ label "@block" . store /[a-zA-Z]+/
        . Build.block_generic
            entry                      (* entry *)
            entry_noindent             (* entry_noindent *)
            entry_noeol                (* entry_noeol *)
            entry_noindent_noeol       (* entry_noindent_noeol *) 
            Util.comment               (* comment *)
            Util.comment_noindent      (* comment_noindent *)
            /[ \t\n]*\{[ \t\n]*/       (* ldelim_re *)
            Build.block_rdelim_re      (* rdelim_re *) 
            " {\n\t"                   (* ldelim_default *) 
            Build.block_rdelim_default (* rdelim_default *) 
        ]

   let lns = ((Util.indent . block)|Util.empty|Util.comment)*

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

   test Bacula.lns get "Storage {\n   Name = kaki-sd\n}" =
      {"@block" = "Storage"
         {"Name" = "kaki-sd"}
      }

   (* value can have quotes *)
   test Bacula.lns get "Storage {\n   Name = \"kaki sd\"\n}" =
      {"@block" = "Storage"
         {"Name" = "kaki sd"}
      }

   (* whitespace in key *)
   test Bacula.lns get "Storage {\n   Pid Directory = kaki sd\n}" =
      {"@block" = "Storage"
         {"Pid Directory" = "kaki sd"}
      }

   (* semicolon *)
   test Bacula.lns get "Storage {\n   Name = kaki-sd;\n}" =
      {"@block" = "Storage"
         {"Name" = "kaki-sd" }
      }

   (* inline comment *)
   test Bacula.lns get "Storage {\n   Name = kaki-sd         # just a comment\n}" =
      {"@block" = "Storage"
         {"Name" = "kaki-sd"
           { "#comment" = "just a comment"} }
      }

   (* multiple values *)
   test Bacula.lns get "Storage {\n  Name = kaki sd\nFoo = moo\n}" =
      {"@block" = "Storage"
         {"Name" = "kaki sd"}
         {"Foo" = "moo"}
      }

   (* newline comment *)
   test Bacula.lns get "Storage {\n  Name = kaki sd\n# just a comment\n}" =
      {"@block" = "Storage"
         {"Name" = "kaki sd" }
         {"#comment" = "just a comment" }
      }

   (* TODO: include statements *)
   test Bacula.lns get "Storage {\n  @/etc/foo.conf\n}" =
      {"@block" = "Storage"
         {"@include" = "/etc/foo.conf"}
      }

   test Bacula.lns get "Storage {\n   Name = kaki sd}" =
      {"@block" = "Storage"
         {"Name" = "kaki sd"}
      }

   (* Blocks can follow each other without \n *)
   test Bacula.lns get "Storage{Name = kaki sd}Storage{Name = kaki-sd}" =
   { "@block" = "Storage"
     { "Name" = "kaki sd" }
   }
   { "@block" = "Storage"
     { "Name" = "kaki-sd" }
   }

   test Bacula.lns get "FileSet { Include { signature = SHA1 } }" =
   { "@block" = "FileSet"
       { "@block" = "Include"
         { "signature" = "SHA1" }
       }
   }
   
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
      {"@block" = "FileSet"
         {"Name" = "DefaultSet"}
         {"@block" = "Include"
            {"@block" = "Options"
              {"signature" = "SHA1"}
              {"noatime" = "yes"}
            }
            { }
            {"File" = "/etc"}
         }
      }

   (* TODO: comment at end of line with } *)
