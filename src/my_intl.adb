------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

package body My_Intl is
   function "+"( Msg: UTF8_String ) return UTF8_String is
   begin
      return Locale_To_UTF8( Gettext( Msg ) );
   end;
end My_Intl;
