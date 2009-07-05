------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Glib.Main;

with Gtk.Handlers; use Gtk.Handlers;
with Gdk.Event; use Gdk.Event;
with Gtk.Button;
with Gtk.Tree_Selection;

package Main_Window_Pkg.Callbacks is

   package Main_Window_Callback is new Gtk.Handlers.Callback
     (Widget_Type => Main_Window_Record);

   package Main_Window_Return_Callback is new Gtk.Handlers.Return_Callback
     (Widget_Type => Main_Window_Record,
      Return_Type => Boolean);

   package Menu_Item_Callback is new Gtk.Handlers.Callback
     ( Widget_Type => Gtk_Menu_Item_Record );

   package Button_Callback is new Gtk.Handlers.Callback
     ( Widget_Type => Gtk.Button.Gtk_Button_Record );

   package Selection_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Tree_Selection.Gtk_Tree_Selection_Record,
      User_Type   => Main_Window_Type);

   function On_Main_Window_Delete (Win : access Main_Window_Record'Class;
                                   Event : Gdk_Event) return Boolean;
   procedure On_Main_Window_Destroy (Win : access Main_Window_Record'Class);
   procedure On_Item_Game_Quit_Pressed (Item : access Gtk_Menu_Item_Record'Class);
   function On_Timeout(Win : in Main_Window_Type) return Boolean;
   procedure On_New_Button_Pressed (Win : access Main_Window_Record'Class);
   procedure On_Properties_Button_Pressed (Win : access Main_Window_Record'Class);
   procedure On_Record_Button_Pressed (Win : access Main_Window_Record'Class);
   procedure On_Pause_Button_Pressed (Win : access Main_Window_Record'Class);
   procedure On_Stop_Button_Pressed (Win : access Main_Window_Record'Class);
   procedure On_Delete_Button_Pressed (Win : access Main_Window_Record'Class);
   procedure On_Table_Selected
     (Selection : access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class;  Win : in Main_Window_Type);
   procedure Update_Tables(Win : in Main_Window_Type);

   package Win_Timeout is
     new Glib.Main.Generic_Sources (Main_Window_Type);

end Main_Window_Pkg.Callbacks;
