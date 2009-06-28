------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with GtkAda.Intl; use GtkAda.Intl;
with Glib.Convert; use Glib.Convert;
with Glib; use Glib;

package My_Intl is

   function "+"( Msg: UTF8_String ) return UTF8_String;

end My_Intl;
