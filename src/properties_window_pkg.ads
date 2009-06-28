------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Gtk.Window;
with Gtk.Button;
with Gtk.GEntry;
with Gtk.Notebook;
with Gtk.Calendar;
with Gtk.Spin_Button;
with Gtk.Tree_View;
with Gtk.List_Store;
with Gtk.Radio_Button;

with Download_Manager;

package Properties_Window_Pkg is

   ------------------------------
   -- Properties_Window_Record --
   ------------------------------

   type Properties_Window_Record is new Gtk.Window.Gtk_Window_Record with
      record
         OK_Button     : Gtk.Button.Gtk_Button;
         Cancel_Button : Gtk.Button.Gtk_Button;
         Calendar      : Gtk.Calendar.Gtk_Calendar;
         Hour          : Gtk.Spin_Button.Gtk_Spin_Button;
         Minute        : Gtk.Spin_Button.Gtk_Spin_Button;
         Freq_View     : Gtk.Tree_View.Gtk_Tree_View;
         Freq          : Gtk.List_Store.Gtk_List_Store;
         Duration      : Gtk.Spin_Button.Gtk_Spin_Button;
         Address       : Gtk.GEntry.Gtk_Entry;
         Prerecorded   : Gtk.Radio_Button.Gtk_Radio_Button;
         Live          : Gtk.Radio_Button.Gtk_Radio_Button;
         Notebook      : Gtk.Notebook.Gtk_Notebook;
         Name_Entry    : Gtk.GEntry.Gtk_Entry;
      end record;

   type Properties_Window is access all Properties_Window_Record'Class;

   Prop_Win : Properties_Window;

   procedure Gtk_new(Win : out Properties_Window);
   procedure Initialize(Win : access Properties_Window_Record'Class );
   procedure Reset(D : in Download_Manager.Download);
   procedure Reset;

end Properties_Window_Pkg;
