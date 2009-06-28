------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Text_IO;
with Ada.Exceptions;
with system;

with Glib;
with Glib.Types;
with Glib.Object;

with Gtk.Handlers;
with Gtk.Button;
with Gdk.Event;
with Gtk.Enums;
with Gdk.Window;
with Gtk.Stock;
with Gtk.Box;
with Gtk.GEntry;
with Gtk.Tree_View;
with Gtk.List_Store;
with Gtk.Tree_Selection;
with Gtk.Notebook;
with Gtk.Tree_Model;
with Gtk.Widget;
with Gtk.Arguments;
with Gtk.Label;
with Gtk.Editable;
with Gtk.Hbutton_Box;
with Gtk.Notebook;
with Gtk.Calendar;
with Gtk.Spin_Button;
with Gtk.Adjustment;
with Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;
with Gtk.Tree_Selection;
with Gtk.Frame;
with Gtk.Radio_Button;

with Gtkada.Dialogs;            use Gtkada.Dialogs;

with Main_Window_Pkg;
with Downloaders;
with My_Intl;                   use My_Intl;
with Utils;                     use Utils;


package body Properties_Window_Pkg is

   The_Download  : Download_Manager.Download;
   Name          : Unbounded_String;
   Name_Entry_Id : Gtk.Handlers.Handler_Id;

   package Callbacks is

      package Prop_Window_Callback is new Gtk.Handlers.Callback
        (Widget_Type => Properties_Window_Record);

      package Prop_Window_Return_Callback is new Gtk.Handlers.Return_Callback
        (Widget_Type => Properties_Window_Record,
         Return_Type => Boolean);

      package Button_Callback is new Gtk.Handlers.Callback
        ( Widget_Type => Gtk.Button.Gtk_Button_Record );

      package Entry_Callback is new Gtk.Handlers.Callback
        (Widget_Type => Gtk.Gentry.Gtk_Entry_Record);

      function On_Prop_Window_Delete (Win   : access Properties_Window_Record'Class;
                                      Event : in     Gdk.Event.Gdk_Event) return Boolean;

      procedure On_OK_Button_Clicked(Button : access Gtk.Button.Gtk_Button_Record'Class );

      procedure On_Cancel_Button_Clicked(Button : access Gtk.Button.Gtk_Button_Record'Class );


      procedure On_Prop_Window_Show(Win : access Properties_Window_Record'Class );

      procedure On_Prop_Window_Hide(Win : access Properties_Window_Record'Class );

      procedure On_Name_Entry_Insert_Text (Object : access Gtk.GEntry.Gtk_Entry_Record'Class;
                                           Params : in Gtk.Arguments.Gtk_Args);

   end Callbacks;

   package body Callbacks is

      Invalid_Name        : exception;
      Invalid_Address     : exception;
      Unsupprted_Protocol : exception;

      ---------------------------
      -- On_Prop_Window_Delete --
      ---------------------------

      function On_Prop_Window_Delete (Win   : access Properties_Window_Record'Class;
                                      Event : in     Gdk.Event.Gdk_Event) return Boolean is
      begin
         Hide( Win );
         return True;
      end;

      --------------------------
      -- On_OK_Button_Clicked --
      --------------------------

      procedure On_OK_Button_Clicked(Button : access Gtk.Button.Gtk_Button_Record'Class ) is
         Dialog_Result : Message_Dialog_Buttons;
         St            : Unbounded_String;
         Is_Name_Valid : Boolean                      := False;
         Row           : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
      begin
         begin
            Is_Name_Valid := To_Unbounded_String(Gtk.GEntry.Get_Text(Prop_Win.Name_Entry)) = To_Unbounded_String(Ada.Directories.Simple_Name(Gtk.GEntry.Get_Text(Prop_Win.Name_Entry)));
         exception
            when Ada.Directories.Name_Error =>
               raise Invalid_Name;
         end;

         if not Download_Manager.Is_Url_Valid(Gtk.GEntry.Get_Text(Prop_Win.Address)) then
            raise Invalid_Address;
         end if;

         if not Download_Manager.Is_Protocol_Supported(Gtk.GEntry.Get_Text(Prop_Win.Address)) then
            raise Unsupprted_Protocol;
         end if;

         Download_Manager.Set_Name(The_Download, Gtk.GEntry.Get_Text(Prop_Win.Name_Entry));
         Download_Manager.Set_Address(The_Download, Gtk.GEntry.Get_Text(Prop_Win.Address));
         Download_Manager.Set_Live(The_Download, Gtk.Radio_Button.Get_Active(Prop_Win.Live));
         declare
            use Ada.Calendar.Formatting;
            use Ada.Calendar.Time_Zones;

            Day   : Glib.Guint;
            Month : Glib.Guint;
            Year  : Glib.Guint;
            Time1 : Ada.Calendar.Time;
         begin
            Gtk.Calendar.Get_Date(Prop_Win.Calendar, Year, Month, Day);
            Time1 := Utils.Local_Time_Of(
                                         Ada.Calendar.Year_Number(Year),
                                         Ada.Calendar.Month_Number(Glib.Guint'Succ(Month)),
                                         Ada.Calendar.Day_Number(Day),
                                         Hour_Number(Gtk.Spin_Button.Get_Value(Prop_Win.Hour)),
                                         Minute_Number(Gtk.Spin_Button.Get_Value(Prop_Win.Minute)),
                                         Second(Download_Manager.Get_Requested_Start_Time(The_Download)));
            Download_Manager.Set_Requested_Start_Time(The_Download, Time1);
         end;

         Gtk.Tree_Selection.Get_Selected(Gtk.Tree_View.Get_Selection(Prop_Win.Freq_View), Gtk.Tree_Model.Gtk_Tree_Model(Prop_Win.Freq), Row);
         Download_Manager.Set_Frequency(The_Download,
                                        Download_Manager.Recurrence'Val( Gtk.List_Store.Get_Int(Prop_Win.Freq, Row, 0)));
         Download_Manager.Set_Minutes(The_Download, Downloaders.Download_Duration(Gtk.Spin_Button.Get_Value(Prop_Win.duration)));
         if Download_Manager.Is_New(The_Download) then
            Download_Manager.Add(The_Download);
         else
            Download_Manager.Modify(Download_Manager.Get_Id(The_Download), The_Download);
         end if;
         Hide( Prop_Win );
      exception
         when Invalid_Name =>
            Dialog_Result := Message_Dialog("Invalid name");
         when Invalid_Address =>
            Dialog_Result := Message_Dialog("Invalid address");
         when Unsupprted_Protocol =>
            Dialog_Result := Message_Dialog("Unsupprted protocol. Currently, the only supported protocol is MMS.");
         when E: others =>
            Echo(Ada.Exceptions.Exception_Information (E));
            Dialog_Result := Message_Dialog(Ada.Exceptions.Exception_Information (E));
      end On_OK_Button_Clicked;

      ------------------------------
      -- On_Cancel_Button_Clicked --
      ------------------------------

      procedure On_Cancel_Button_Clicked( Button : access Gtk.Button.Gtk_Button_Record'Class ) is
      begin
         Hide( Prop_Win );
      end On_Cancel_Button_Clicked;

      -------------------------
      -- On_Prop_Window_Show --
      -------------------------

      procedure On_Prop_Window_Show( Win : access Properties_Window_Record'Class ) is
         use Ada.Calendar.Formatting;
         use Ada.Calendar.Time_Zones;
         use Download_Manager;

         Booly                : Boolean;
         Selection            : Gtk.Tree_Selection.Gtk_Tree_Selection;
         Requested_Start_Time : Ada.Calendar.Time                   :=
           Download_Manager.Get_Requested_Start_Time(The_Download);
         Is_Live              : Boolean                             :=
           Download_Manager.Is_Live(The_Download);
         Offset               : Ada.Calendar.Time_Zones.Time_Offset :=
           Ada.Calendar.Time_Zones.UTC_Time_Offset(Requested_Start_Time);
         The_State            : Download_Manager.State              :=
           Download_Manager.Get_State(The_Download);
         Is_Sleeping          : Boolean                             :=
           The_State = Download_Manager.Sleeping;
      begin
         Gtk.GEntry.Set_Text(Win.Name_Entry, Download_Manager.Get_Name(The_Download));
         Gtk.GEntry.Set_Editable (Win.Name_Entry, Is_Sleeping);

         Gtk.GEntry.Set_Text(Win.Address, Download_Manager.Get_Address(The_Download));
         Gtk.GEntry.Set_Editable (Win.Address, Is_Sleeping);

         Gtk.Radio_Button.Set_Active(Win.Prerecorded, not Is_Live);
         Gtk.Radio_Button.Set_Active(Win.Live, Is_Live);

         Gtk.Radio_Button.Set_Sensitive(Win.Prerecorded, False);
         Gtk.Radio_Button.Set_Sensitive(Win.Live, Is_Sleeping);

         Booly := Gtk.Calendar.Select_Month(Win.Calendar, Glib.Guint(Ada.Calendar.Month_Number'Pred(Month(Requested_Start_Time, Offset))), Glib.Guint(Year(Requested_Start_Time, Offset)));
         Gtk.Calendar.Select_Day(Win.Calendar, Glib.Guint(Day(Requested_Start_Time, Offset)));
         Gtk.Spin_Button.Set_Value(Win.Hour, Glib.Gdouble(Hour(Requested_Start_Time, Offset)));
         Gtk.Spin_Button.Set_Value(Win.Minute, Glib.Gdouble(Minute(Requested_Start_Time, Offset)));

         Gtk.Calendar.Set_Sensitive(Win.Calendar, Is_Sleeping);
         Gtk.Spin_Button.Set_Sensitive(Win.Hour, Is_Sleeping);
         Gtk.Spin_Button.Set_Sensitive(Win.Minute, Is_Sleeping);

         Selection := Gtk.Tree_View.Get_Selection(Win.Freq_View);
         Gtk.Tree_Selection.Select_Iter(Selection, Gtk.List_Store.Nth_Child(Win.Freq,
           Gtk.Tree_Model.Null_Iter,
           Download_Manager.Recurrence'Pos(Download_Manager.Get_Frequency(The_Download))));
         Gtk.Tree_View.Set_Sensitive(Win.Freq_View, Is_Sleeping);

         Gtk.Spin_Button.Set_Value(Win.Duration, Glib.Gdouble(Download_Manager.Get_Minutes(The_Download)));
         Gtk.Spin_Button.Set_Sensitive(Win.Duration, Download_Manager.Is_Duration_Modifiable(The_Download));
      end On_Prop_Window_Show;

      -------------------------
      -- On_Prop_Window_Hide --
      -------------------------

      procedure On_Prop_Window_Hide( Win : access Properties_Window_Record'Class ) is
      begin
         null;
      end;

      X : Integer := 0;

      -------------------------------
      -- On_Name_Entry_Insert_Text --
      -------------------------------

      procedure On_Name_Entry_Insert_Text (Object : access Gtk.GEntry.Gtk_Entry_Record'Class; Params : Gtk.Arguments.Gtk_Args) is
      begin
         -- TODO: filter unwanted chars
         null;
      end On_Name_Entry_Insert_Text;

   end Callbacks;

   use Callbacks;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New( Win : out Properties_Window ) is
   begin
      Win := new Properties_Window_Record;
      Initialize( Win );
   end;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize( Win : access Properties_Window_Record'Class ) is
      lbl       : Gtk.Label.Gtk_Label;
      Box1      : Gtk.Box.Gtk_Box;
      Box2      : Gtk.Hbutton_Box.Gtk_Hbutton_Box;
      Box3      : Gtk.Box.Gtk_Box;
      Column_No : Glib.GInt;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Text      : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Frame     : Gtk.Frame.Gtk_Frame;
      Adj       : Gtk.Adjustment.Gtk_Adjustment;
      Row       : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
   begin
      Gtk.Window.Initialize( Win, Gtk.Enums.Window_Toplevel );
      Set_Title( Win, +"Properties" );
      Set_Transient_For( Win, Main_Window_Pkg.Main_Window );
      Set_Modal( Win, True );
      Set_Type_Hint( Win, Gdk.Window.Window_Type_Hint_Dialog );
      Set_Position( Win, Gtk.Enums.Win_Pos_Center_Always );

      Gtk.Notebook.Gtk_New(Win.Notebook);

      Gtk.GEntry.Gtk_New( Win.Name_Entry);
      Gtk.GEntry.Set_Text(Win.Name_Entry, "default");
      Gtk.GEntry.Gtk_New( Win.Address);

      Gtk.Radio_Button.Gtk_New(Win.Prerecorded, Label => "Prerecorded");
      Gtk.Radio_Button.Gtk_New(Win.Live, Group => Gtk.Radio_Button.Get_Group(Win.Prerecorded), Label => "Live");

      Gtk.Adjustment.Gtk_New(Adj,
                             30.0,
                             Glib.Gdouble(Downloaders.Download_Duration'First),
                             Glib.Gdouble(Downloaders.Download_Duration'Last),
                             1.0,
                             60.0,
                             0.0);

      Gtk.Spin_Button.Gtk_New(Win.Duration, Adj, 0.5, 0);
      Gtk.Calendar.Gtk_New(Win.Calendar);
      Gtk.Spin_Button.Gtk_New(Win.Hour, 0.0, 23.0, 1.0);
      Gtk.Spin_Button.Gtk_New(Win.Minute, 0.0, 59.0, 1.0);

      Gtk.Tree_View.Gtk_New (Win.Freq_View);
      Gtk.List_Store.Gtk_New (Win.Freq, (0 => Glib.GType_Int, 1 => Glib.GType_String));
      Gtk.Tree_View.Set_Model (Win.Freq_View, Win.Freq.all'Access);
      Gtk.Tree_View_Column.Gtk_New (Column);
      Gtk.Tree_View.Set_Headers_Visible (Win.Freq_View, False);
      Gtk.Tree_View_Column.Set_Resizable (Column, False);
      Column_No := Gtk.Tree_View.Append_Column (Win.Freq_View, Column);
      Gtk.Cell_Renderer_Text.Gtk_New (Text);
      Gtk.Tree_View_Column.Pack_Start (Column, Text, True);
      Gtk.Tree_View_Column.Add_Attribute (Column, Text, "text", 1);

      For R in Download_Manager.Recurrence loop

         Gtk.List_Store.Append(Win.Freq, Row);
         Gtk.List_Store.Set (Win.Freq, Row, 0, Glib.Gint(Download_Manager.Recurrence'Pos(R)));
         Gtk.List_Store.Set (Win.Freq, Row, 1, Download_Manager.Recurrence'Image(R));
      end loop;

      Gtk.Button.Gtk_New_From_Stock( Win.OK_Button, Gtk.Stock.Stock_OK );
      Gtk.Button.Gtk_New_From_Stock( Win.Cancel_Button, Gtk.Stock.Stock_Cancel );

      Gtk.Box.Gtk_New_Vbox( Box3);
      Gtk.Box.Pack_Start(Box3, Win.Name_Entry, False, False, 2);
      Gtk.Box.Pack_Start(Box3, Win.Address, False, False, 2);
      Gtk.Box.Gtk_New_Hbox( Box1);
      Gtk.Box.Pack_Start(Box1, Win.Prerecorded, False, False, 2);
      Gtk.Box.Pack_Start(Box1, Win.Live, False, False, 2);
      Gtk.Box.Pack_Start(Box3, Box1, False, False, 10);

      Gtk.Box.Pack_Start(Box3, Win.Duration, False, False, 2);

      Gtk.Label.Gtk_New(lbl, "General");
      Gtk.Notebook.Append_Page(Win.Notebook, Box3, lbl);

      Gtk.Box.Gtk_New_Vbox( Box3);
      Gtk.Box.Pack_Start(Box3, Win.Calendar, False, False, 2);

      Gtk.Box.Gtk_New_Hbox( Box1);
      Gtk.Box.Pack_Start(Box1, Win.Hour, False, False, 2);
      Gtk.Box.Pack_Start(Box1, Win.Minute, False, False, 2);
      Gtk.Box.Pack_Start(Box3, Box1, False, False, 10);

      Gtk.Box.Gtk_New_Hbox( Box1);
      Gtk.Frame.Gtk_New(Frame, "Frequency");
      Gtk.Frame.Add(Frame, Win.Freq_View);
      Gtk.Box.Pack_Start(Box1, Frame, False, False, 2);
      Gtk.Box.Pack_Start(Box3, Box1, False, False, 2);

      Gtk.Label.Gtk_New(lbl, "Schedule");
      Gtk.Notebook.Append_Page(Win.Notebook, Box3, lbl);

      Gtk.Hbutton_Box.Gtk_New( Box2);
      Gtk.Hbutton_Box.Set_Layout (Box2, Gtk.Enums.Buttonbox_Start);
      Gtk.Hbutton_Box.Set_Spacing (Box2, 5);
      Gtk.Hbutton_Box.Pack_Start(Box2, Win.OK_Button);
      Gtk.Hbutton_Box.Pack_Start(Box2, Win.Cancel_Button);

      Gtk.Box.Gtk_New_Vbox( Box1);
      Gtk.Box.Pack_Start(Box1, Win.Notebook, False, False, 2);
      Gtk.Box.Pack_Start(Box1, Box2, False, False, 2);

      Add( Win, Box1 );

      Button_Callback.Connect( Win.OK_Button, "clicked",
                              Button_Callback.To_Marshaller( On_OK_Button_Clicked'access ) );

      Button_Callback.Connect( Win.Cancel_Button, "clicked",
                              Button_Callback.To_Marshaller( On_Cancel_Button_Clicked'access ) );


      Prop_Window_Callback.Connect( Win, "show",
                                   Prop_Window_Callback.To_Marshaller( On_Prop_Window_Show'access ) );

      Prop_Window_Callback.Connect( Win, "hide",
                                   Prop_Window_Callback.To_Marshaller( On_Prop_Window_Hide'access ) );

      Prop_Window_Return_Callback.Connect( Win, "delete_event",
                                          Prop_Window_Return_Callback.To_Marshaller( On_Prop_Window_Delete'Access ) );

      Name_Entry_Id := Entry_Callback.Connect( Win.Name_Entry, "insert_text",
                                              On_Name_Entry_Insert_Text'access , False );
   end;

   -----------
   -- Reset --
   -----------

   procedure Reset(D : in Download_Manager.Download) is
   begin
      The_Download := D;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Download_Manager.Init(The_Download);
   end Reset;

end Properties_Window_Pkg;
