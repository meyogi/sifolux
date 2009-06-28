------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Directories;

with GNAT.OS_Lib;

package Config is
   Home_Path       : constant String := GNAT.OS_Lib.Getenv ("HOME").all;
   Config_Dir      : constant String := ".sifolux";
   Config_Path     : constant String := Ada.Directories.Compose(Home_Path,   Config_Dir);
   Info_List_Path  : constant String := Ada.Directories.Compose(Config_Path, "info");
   Base_Local_Path : constant String := Ada.Directories.Compose(Home_Path,   "sifolux_downloads");
   Lock_Full_Name  : constant String := Ada.Directories.Compose(Config_Path, "app.lock");

   procedure Init;

end Config;
