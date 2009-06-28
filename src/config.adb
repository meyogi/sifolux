------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Directories;

package body Config is

   procedure Init is
   begin
      Ada.Directories.Create_Path(Config_Path);
      Ada.Directories.Create_Path(Info_List_Path);
      Ada.Directories.Create_Path(Base_Local_Path);
   end Init;

end Config;
