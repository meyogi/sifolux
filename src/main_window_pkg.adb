------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Text_IO;

with GLib;                      use GLib;
with Glib.Values;
with Glib.Main;
with Glib.Properties;

with Gtk.Enums;
with Gtk.Window;
with Gtk.Cell_Renderer_Text;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Tree_Model;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.List_Store;
with Gtk.Button;
with Gtk.Widget;
with Gdk.Color;
with Gtk.Handlers;
with Gtk.Hbutton_Box;
with Gtk.Scrolled_Window;
with Gtk.Container;
with Gtk.Stock;
with Gtk.Main;

with Main_Window_Pkg.Callbacks; use Main_Window_Pkg.Callbacks;
with Properties_Window_Pkg;
with Download_Manager;
with My_Intl;                   use My_Intl;

package body Main_Window_Pkg is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New(Win : out Main_Window_Type ) is
   begin
      Win := new Main_Window_Record;
      Main_Window_Pkg.Initialize( Win );
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize(Win : in Main_Window_Type ) is
      use Main_Window_Return_Callback;
      use Main_Window_Callback;
      use Menu_Item_Callback;

      Column_No       : GInt;
      Column          : Gtk_Tree_View_Column;
      Text            : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Box1            : Gtk.Box.Gtk_Box;
      Box2            : Gtk.Hbutton_Box.Gtk_Hbutton_Box;
      Box3            : Gtk.Box.Gtk_Box;
      Scroller        : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Button          : Gtk.Button.Gtk_Button;
      Changes_Map     : Download_Manager.Download_Summary_Maps.Map;
      Timeout_Handler : Glib.Main.G_Source_Id;
      Row             : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
   begin
      Gtk.Window.Initialize( Win, Gtk.Enums.Window_Toplevel );
      Set_Default_Size(Win, 500, 500);

      Gtk_New(Win.File_Menu);
      Gtk_New_With_Mnemonic( Win.Item_Quit, "_" & ( +"Quit") );
      Append(Win.File_Menu, Win.Item_Quit);
      Connect( Win.Item_Quit, "activate",
              Menu_Item_Callback.To_Marshaller( On_Item_Game_Quit_Pressed'Access ) );
      Gtk_New( Win.Menu_Bar );
      Gtk_New_With_Mnemonic( Win.Item_File, "_" & (+"File") );
      Set_Submenu( Win.Item_File, Win.File_Menu );
      Append( Win.Menu_Bar, Win.Item_File);

      Gtk_New (Win.Table_View);
      Gtk.List_Store.Gtk_New (Win.Table, (0 => GType_Int,
                                          1 => GType_String,
                                          2 => GType_String,
                                          3 => GType_Int,
                                          4 => GType_String));

      Set_Model (Win.Table_View, Win.Table.all'Access);
      Gtk_New (Column);
      Set_Title (Column, "Start");
      Set_Resizable (Column, True);
      Column_No := Append_Column (Win.Table_View, Column);
      Gtk.Cell_Renderer_Text.Gtk_New (Text);
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 1);

      Gtk_New (Column);
      Set_Title (Column, "Name");
      Set_Resizable (Column, True);
      Column_No := Append_Column (Win.Table_View, Column);
      Gtk.Cell_Renderer_Text.Gtk_New (Text);
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 2);

      Gtk_New (Column);
      Set_Title (Column, "Bytes");
      Set_Resizable (Column, True);
      Column_No := Append_Column (Win.Table_View, Column);
      Gtk.Cell_Renderer_Text.Gtk_New (Text);
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 3);

      Gtk_New (Column);
      Set_Title (Column, "State");
      Set_Resizable (Column, True);
      Column_No := Append_Column (Win.Table_View, Column);
      Gtk.Cell_Renderer_Text.Gtk_New (Text);
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 4);

      Gtk.Scrolled_Window.Gtk_New(Scroller);
      Gtk.Scrolled_Window.Add(Scroller, Win.Table_View);

      Gtk_New_Vbox( Box1);
      Pack_Start(Box1, Win.Menu_Bar, False, False, 2);

      Gtk.Hbutton_Box.Gtk_New( Box2);
      Gtk.Hbutton_Box.Set_Layout (Box2, Gtk.Enums.Buttonbox_Start);
      Gtk.Hbutton_Box.Set_Spacing (Box2, 1);

      Gtk.Button.Gtk_New_From_Stock( Win.Pause_Button, Gtk.Stock.Stock_Media_Pause );
      Gtk.Hbutton_Box.Pack_Start(Box2, Win.Pause_Button, False, False);

      Gtk.Button.Gtk_New_From_Stock( Win.Stop_Button, Gtk.Stock.Stock_Media_Stop );
      Gtk.Hbutton_Box.Pack_Start(Box2, Win.Stop_Button, False, False);

      Gtk.Button.Gtk_New_From_Stock( Win.Record_Button, Gtk.Stock.Stock_Media_Record );
      Gtk.Hbutton_Box.Pack_Start(Box2, Win.Record_Button, False, False);

      Pack_Start(Box1, Box2, False, False);

      Pack_Start(Box1, Scroller);

      Gtk.Hbutton_Box.Gtk_New( Box2);
      Gtk.Hbutton_Box.Set_Layout (Box2, Gtk.Enums.Buttonbox_Start);
      Gtk.Hbutton_Box.Set_Spacing (Box2, 5);
      Gtk.Button.Gtk_New_From_Stock( Win.New_Button, Gtk.Stock.Stock_Add );

      Gtk.Hbutton_Box.Pack_Start(Box2, Win.New_Button, False, False);
      Gtk.Button.Gtk_New_From_Stock( Win.Properties_Button, Gtk.Stock.Stock_Properties );

      Gtk.Button.Set_Sensitive (Win.Properties_Button, False);
      Gtk.Hbutton_Box.Pack_Start(Box2, Win.Properties_Button, False, False);
      Gtk_New_Hbox( Box3);
      Gtk.Button.Gtk_New_From_Stock( Button, Gtk.Stock.Stock_Delete );
      Gtk.Button.Set_Sensitive (Button, False);

      Gtk.Hbutton_Box.Set_Layout (Box2, Gtk.Enums.Buttonbox_Start);
      Gtk.Hbutton_Box.Pack_End(Box2, Button, True, False);
      Pack_Start(Box1, Box2, False, False);

      Add (Win, Box1);

      Set_Title( Win, "Sifolux" );

      Connect( Win, "delete_event",
              Main_Window_Return_Callback.To_Marshaller (On_Main_Window_Delete'Access) );

      Main_Window_Callback.Connect( Win, "destroy",
                                   Main_Window_Callback.To_Marshaller (On_Main_Window_Destroy'Access) );

      Main_Window_Callback.Object_Connect
        (Win.New_Button,
         "clicked",
         Main_Window_Callback.To_Marshaller (On_New_Button_Pressed'Access),
         Slot_Object => Win);

      Main_Window_Callback.Object_Connect
        (Win.Properties_Button,
         "clicked",
         Main_Window_Callback.To_Marshaller (On_Properties_Button_Pressed'Access),
         Slot_Object => Win);

      Main_Window_Callback.Object_Connect
        (Win.Record_Button,
         "clicked",
         Main_Window_Callback.To_Marshaller (On_Record_Button_Pressed'Access),
         Slot_Object => Win);

      Main_Window_Callback.Object_Connect
        (Win.Pause_Button,
         "clicked",
         Main_Window_Callback.To_Marshaller (On_Pause_Button_Pressed'Access),
         Slot_Object => Win);

      Main_Window_Callback.Object_Connect
        (Win.Stop_Button,
         "clicked",
         Main_Window_Callback.To_Marshaller (On_Stop_Button_Pressed'Access),
         Slot_Object => Win);

      Selection_Callback.Connect
        (Get_Selection (Win.Table_View), "changed",
         Selection_Callback.To_Marshaller (On_Table_Selected'Access),
         Win);

      Timeout_Handler := Win_Timeout.Timeout_Add(1000, Main_Window_Pkg.Callbacks.On_Timeout'Access, Win);
   end Initialize;

end Main_Window_Pkg;
