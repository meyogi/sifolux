------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Gtk.Window;    use Gtk.Window;
with Gtk.Menu_Bar;  use Gtk.Menu_Bar;
with Gtk.Menu;      use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Notebook;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Box;       use Gtk.Box;
with Gtk.Button;
with Gtk.List_Store;

package Main_Window_Pkg is

   ------------------------
   -- Main_Window_Record --
   ------------------------

   type Main_Window_Record is new Gtk_Window_Record with
      record
         Menu_Bar          : Gtk_Menu_Bar;
         File_Menu         : Gtk_Menu;
         Table_View        : Gtk_Tree_View;
         Table             : Gtk.List_Store.Gtk_List_Store;
         Item_File         : Gtk_Menu_Item;
         Item_Quit         : Gtk_Menu_Item;
         Record_Button     : Gtk.Button.Gtk_Button;
         Pause_Button      : Gtk.Button.Gtk_Button;
         Stop_Button       : Gtk.Button.Gtk_Button;
         Delete_Button     : Gtk.Button.Gtk_Button;
         Properties_Button : Gtk.Button.Gtk_Button;
         New_Button        : Gtk.Button.Gtk_Button;
      end record;

   type Main_Window_Type is access all Main_Window_Record'Class;

   type Main_Window_Access is access all Main_Window_Record;

   Main_Window: Main_Window_Type;

   procedure Gtk_New(Win : out Main_Window_Type);
   procedure Initialize(Win : in Main_Window_Type);
private
end Main_Window_Pkg;
