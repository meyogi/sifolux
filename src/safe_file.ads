------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Text_IO;

package Safe_File is

   Extension_New : constant String := "new";
   Extension_Old : constant String := "old";

   procedure Clean(Path : in String);
   procedure Open(File : in out Text_IO.File_Type;
                  Name : in     String;
                  Form : in     String := "");
   procedure Close(File : in out Text_IO.File_Type);
   function Is_Open(File : in Text_IO.File_Type) return Boolean;
   procedure Close_Without_Update(File : in out Text_IO.File_Type);

end Safe_File;
