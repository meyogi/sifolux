------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Directories;       use  Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Safe_File is

   -- TODO: should be changed so that files that were opened using this package,
   --       must be closed using this package

   --------------
   -- Name_New --
   --------------

   function Name_New(The_Original_Name : in String) return String is
   begin
      return Ada.Directories.Compose(Containing_Directory(The_Original_Name),
                                     Simple_Name(The_Original_Name),
                                     Extension_New);
   end Name_New;

   --------------
   -- Name_Old --
   --------------

   function Name_Old(The_Original_Name : in String) return String is
   begin
      return Ada.Directories.Compose(Containing_Directory(The_Original_Name),
                                     Simple_Name(The_Original_Name),
                                     Extension_Old);
   end Name_Old;

   -------------------
   -- Original_Name --
   -------------------

   function Original_Name(Name2 : in String) return String is
   begin
      return Ada.Directories.Compose(Containing_Directory(Name2),
                                     Base_Name(Name2));
   end Original_Name;

   -----------
   -- Clean --
   -----------

   procedure Clean (Path : in String) is
      Search          : Ada.Directories.Search_Type;
      Directory_Entry : Ada.Directories.Directory_Entry_Type;
   begin
      Ada.Directories.Start_Search(Search,
                                   Path,
                                   "*." & Extension_New,
                                   (False, True, False));
      while Ada.Directories.More_Entries(Search) loop
         Ada.Directories.Get_Next_Entry(Search, Directory_Entry);
         declare
            The_Name_New      : String := Ada.Directories.Full_Name(Directory_Entry);
            The_Original_Name : String := Original_Name(The_Name_New);
            The_Name_Old      : String := Name_Old(The_Original_Name);
         begin
            if Ada.Directories.Exists(The_Name_Old) then
               if Ada.Directories.Exists(The_Original_Name) then
                  Ada.Directories.Delete_File(The_Name_Old);
               else
                  Ada.Directories.Rename(The_Name_New, The_Original_Name);
               end if;
            else
               Ada.Directories.Delete_File(The_Name_New);
            end if;
         end;
      end loop;
      Ada.Directories.End_Search(Search);

      Ada.Directories.Start_Search(Search,
                                   Path,
                                   "*." & Extension_Old,
                                   (False, True, False));
      while Ada.Directories.More_Entries(Search) loop
         Ada.Directories.Get_Next_Entry(Search, Directory_Entry);
         declare
            The_Name_Old      : String := Ada.Directories.Full_Name(Directory_Entry);
            The_Original_Name : String := Original_Name(The_Name_Old);
         begin
            if Ada.Directories.Exists(The_Original_Name) then
               Ada.Directories.Delete_File(The_Name_Old);
            else
               Ada.Directories.Rename(The_Name_Old, The_Original_Name);
            end if;
         end;
      end loop;
      Ada.Directories.End_Search(Search);
   end Clean;

   ----------
   -- Open --
   ----------

   procedure Open(File : in out Text_IO.File_Type;
                  Name : in String;
                  Form : in String := "") is
      The_Original_Name : String renames Name;
      The_Name_New      : String := Name_New(The_Original_Name);
   begin
      if Ada.Directories.Exists(The_Name_New) then
         Text_IO.Open(File, Text_IO.Out_File, The_Name_New, Form);
      else
         Text_IO.Create (File, Text_IO.Out_File, The_Name_New, Form);
      end if;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close(File : in out Text_IO.File_Type) is
      The_Name_New      : String := Text_IO.Name(File);
      The_Original_Name : String := Original_Name(The_Name_New);
      The_Name_Old      : String := Name_Old(The_Original_Name);
   begin
      if Ada.Directories.Exists(The_Name_Old) then
         if Ada.Directories.Exists(The_Original_Name) then
            Ada.Directories.Delete_File(The_Name_Old);
         else
            Ada.Directories.Rename(The_Name_Old, The_Original_Name);

         end if;
      end if;

      Text_IO.Close(File);
      begin
         if Ada.Directories.Exists(The_Original_Name) then
            Ada.Directories.Rename(The_Original_Name, The_Name_Old);
         end if;

         Ada.Directories.Rename(The_Name_New, The_Original_Name);

         if Ada.Directories.Exists(The_Name_Old) then
            Ada.Directories.Delete_File(The_Name_Old);
         end if;
      exception
            -- all exceptions after File was closed, should be handled here,
            -- otherwise, the user might think the file is still open.
         when others =>
            null;
      end;
   end Close;

   -------------
   -- Is_Open --
   -------------

   function Is_Open(File : in Text_IO.File_Type) return Boolean is
   begin
      return Text_IO.Is_Open(File);
   end Is_Open;

   --------------------------
   -- Close_Without_Update --
   --------------------------

   procedure Close_Without_Update(File : in out Text_IO.File_Type) is
      The_Name_New      : String := Text_IO.Name(File);
      The_Original_Name : String := Original_Name(The_Name_New);
      The_Name_Old      : String := Name_Old(The_Original_Name);

   begin
      if Ada.Directories.Exists(The_Name_Old) then
         if Ada.Directories.Exists(The_Original_Name) then
            Ada.Directories.Delete_File(The_Name_Old);
         else
            Ada.Directories.Rename(The_Name_Old, The_Original_Name);
         end if;
      end if;
      Text_IO.Close(File);
   end Close_Without_Update;

end Safe_File;
