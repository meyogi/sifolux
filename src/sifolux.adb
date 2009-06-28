------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Text_IO;
with Ada.Real_Time;
with Ada.Exceptions;

with GNAT.Lock_Files;

with Gtk.Main;
with Gtkada.Dialogs;

with Main_Window_Pkg;
with Properties_Window_Pkg;
with Download_Manager;
with Config;
with Utils; use Utils;

procedure Sifolux is

   Dialog_Result   : Gtkada.Dialogs.Message_Dialog_Buttons;
   Already_Running : Boolean := False;

begin

   Config.Init;
   Gtk.Main.Init;
   begin
      GNAT.Lock_Files.Lock_File( Config.Lock_Full_Name, Wait => 2.0, Retries => 3 );
      Already_Running := False;

   exception
      when GNAT.Lock_Files.LOCK_ERROR =>
         Already_Running := True;
   end;

   if not Already_Running then
      Download_Manager.Run;
      Main_Window_Pkg.Gtk_New( Main_Window_Pkg.Main_Window );
      Properties_Window_Pkg.Gtk_New( Properties_Window_Pkg.Prop_Win );
      Main_Window_Pkg.Show_All( Main_Window_Pkg.Main_Window );
      Gtk.Main.Main;
      GNAT.Lock_Files.Unlock_File( Config.Lock_Full_Name );
   else
      begin
         Download_Manager.Quit;
         Dialog_Result := Gtkada.Dialogs.Message_Dialog("It seems that another instance of this application is already running."
                                         & ASCII.CR & ASCII.LF
                                         & "If not, delete the file " & Config.Lock_Full_Name);
      exception
         when others =>
            null;
      end;
   end if;

exception
   when others =>
      GNAT.Lock_Files.Unlock_File( Config.Lock_Full_Name );

end Sifolux;
