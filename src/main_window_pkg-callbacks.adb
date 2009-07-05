------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Text_IO;
with Ada.Containers.Vectors;
with Ada.Real_Time;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gtk.Main;
with Gtk.Tree_Model;
with Gtk.List_Store;
with Gtk.Tree_View;
with Gtk.Tree_Selection;
with Gtk.Notebook;

with Properties_Window_Pkg;
with Utils; use Utils;
with Download_Manager;

package body Main_Window_Pkg.Callbacks is

   package Download_Id_Vectors is new Ada.Containers.Vectors
     (Element_Type => Download_Manager.Download_Id,
      Index_Type   => Positive);

   --------------------
   -- Update_Buttons --
   --------------------

   procedure Update_Buttons(Win : in Main_Window_Type) is
      use Gtk.Tree_Model;
      use Download_Manager;

      Tree                      : Gtk.Tree_Model.Gtk_Tree_Model;
      Id                        : Download_Manager.Download_Id;
      Selected_Made             : Boolean;
      The_State                 : Download_Manager.State;
      D                         : Download_Manager.Download;
      Row                       : Gtk_Tree_Iter := Null_Iter;
      Record_Button_Enabled     : Boolean       := False;
      Properties_Button_Enabled : Boolean       := False;
      Pause_Button_Enabled      : Boolean       := False;
      Stop_Button_Enabled       : Boolean       := False;
      Delete_Button_Enabled     : Boolean       := False;
   begin
      Gtk.Tree_Selection.Get_Selected(Gtk.Tree_View.Get_Selection(Win.Table_View), Tree, Row);
      Selected_Made := Row /= Null_Iter;
      if Selected_Made then
         Id        := Download_Manager.Download_Id(Gtk.List_Store.Get_Int(Win.Table, Row, 0));
         D         := Download_Manager.Get(Id);
         The_State :=   Download_Manager.Get_State(D);
      end if;

      Record_Button_Enabled := Selected_Made and then
        (The_State = Sleeping
         or
           The_State = Paused
         or
           The_State = Stopped
         or
           The_State = Finished
         or
           The_State = Missed
        );

      Pause_Button_Enabled := Selected_Made and then
        (The_State = Waiting or The_State = Downloading) ;

      Stop_Button_Enabled := Selected_Made
        and then Download_Manager.Is_Live(D)
        and then (The_State = Waiting or The_State = Downloading or The_State = Paused) ;

      Delete_Button_Enabled := Selected_Made
        and then Download_Manager.Is_Deletable(D);

      Properties_Button_Enabled := Selected_Made;
      Gtk.Button.Set_Sensitive (Win.Record_Button, Record_Button_Enabled);
      Gtk.Button.Set_Sensitive (Win.Pause_Button, Pause_Button_Enabled);
      Gtk.Button.Set_Sensitive (Win.Stop_Button, Stop_Button_Enabled);
      Gtk.Button.Set_Sensitive (Win.Properties_Button, Properties_Button_Enabled);
      Gtk.Button.Set_Sensitive (Win.Delete_Button, Delete_Button_Enabled);

   end Update_Buttons;

   ----------------------------
   -- On_Main_Window_Delete  --
   ----------------------------

   function On_Main_Window_Delete (Win   : access Main_Window_Record'Class;
                                   Event : in     Gdk_Event) return Boolean is
   begin
      Download_Manager.Quit;
      return False;
   end On_Main_Window_Delete;

   ----------------------------
   -- On_Main_Window_Destroy --
   ----------------------------

   procedure On_Main_Window_Destroy (Win : access Main_Window_Record'Class) is
   begin
      Download_Manager.Quit;
      Gtk.Main.Main_Quit;
   end On_Main_Window_Destroy;

   -------------------------------
   -- On_Item_Game_Quit_Pressed --
   -------------------------------

   procedure On_Item_Game_Quit_Pressed (Item : access Gtk_Menu_Item_Record'Class) is
   begin
      Download_Manager.Quit;
      Gtk.Main.Main_Quit;
   end On_Item_Game_Quit_Pressed;

   ----------------------------
   -- On_New_Button_Pressed  --
   ----------------------------

   procedure On_New_Button_Pressed (Win : access Main_Window_Record'Class) is
   begin
      Properties_Window_Pkg.Reset;
      Properties_Window_Pkg.Show_All( Properties_Window_Pkg.Prop_Win );
   end On_New_Button_Pressed;

   ----------------------------------
   -- On_Properties_Button_Pressed --
   ----------------------------------

   procedure On_Properties_Button_Pressed (Win : access Main_Window_Record'Class) is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;

      Tree : Gtk.Tree_Model.Gtk_Tree_Model;
      Id   : Download_Manager.Download_Id;
      D    : Download_Manager.Download;
      Row  : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
   begin
      Gtk.Tree_Selection.Get_Selected(Gtk.Tree_View.Get_Selection(Win.Table_View), Tree, Row);
      if (Row /= Gtk.Tree_Model.Null_Iter) then
         Id := Download_Manager.Download_Id(Gtk.List_Store.Get_Int(Win.Table, Row, 0));
         D  := Download_Manager.Get(Id);
         Properties_Window_Pkg.Reset( D );
         Properties_Window_Pkg.Show_All( Properties_Window_Pkg.Prop_Win );
      end if;
   end On_Properties_Button_Pressed;

   ------------------------------
   -- On_Delete_Button_Pressed --
   ------------------------------

   procedure On_Delete_Button_Pressed (Win : access Main_Window_Record'Class) is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;

      Downloads_Table : Gtk.List_Store.Gtk_List_Store renames Win.Table;
      Tree            : Gtk.Tree_Model.Gtk_Tree_Model;
      Id              : Download_Manager.Download_Id;
      Row             : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
   begin
      Gtk.Tree_Selection.Get_Selected(Gtk.Tree_View.Get_Selection(Win.Table_View), Tree, Row);
      if (Row /= Gtk.Tree_Model.Null_Iter) then
         Id := Download_Manager.Download_Id(Gtk.List_Store.Get_Int(Win.Table, Row, 0));
         Download_Manager.Delete(Id);
         if not Download_Manager.Exists(Id) then
            Gtk.List_Store.Remove(Downloads_Table, Row);
            Update_Tables(Main_Window);
         end if;
      end if;
   end On_Delete_Button_Pressed;

   ------------------------------
   -- On_Record_Button_Pressed --
   ------------------------------

   procedure On_Record_Button_Pressed (Win : access Main_Window_Record'Class) is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use Download_Manager;

      Tree              : Gtk.Tree_Model.Gtk_Tree_Model;
      Id                : Download_Manager.Download_Id;
      Selected_Download : Download_Manager.Download;
      D                 : Download_Manager.Download;
      Current_State     : Download_Manager.State;
      Row               : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
   begin
      Gtk.Tree_Selection.Get_Selected(Gtk.Tree_View.Get_Selection(Win.Table_View), Tree, Row);
      if (Row /= Gtk.Tree_Model.Null_Iter) then
         Id := Download_Manager.Download_Id(Gtk.List_Store.Get_Int(Win.Table, Row, 0));
         Selected_Download := Download_Manager.Get(Id);
         Current_State := Download_Manager.Get_State(Selected_Download);
         case Current_State is
            when Paused =>
               Download_Manager.Start(Id);
            when Sleeping | Stopped | Missed | Finished | Failed =>
               Download_Manager.Init_Like(D, Selected_Download);
               Properties_Window_Pkg.Reset( D );
               Properties_Window_Pkg.Show_All( Properties_Window_Pkg.Prop_Win );
            when others =>
               null;
         end case;
      end if;
   end On_Record_Button_Pressed;

   -----------------------------
   -- On_Pause_Button_Pressed --
   -----------------------------

   procedure On_Pause_Button_Pressed (Win : access Main_Window_Record'Class) is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;

      Tree : Gtk.Tree_Model.Gtk_Tree_Model;
      Id   : Download_Manager.Download_Id;
      Row  : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
   begin
      Gtk.Tree_Selection.Get_Selected(Gtk.Tree_View.Get_Selection(Win.Table_View), Tree, Row);
      if (Row /= Gtk.Tree_Model.Null_Iter) then
         Id := Download_Manager.Download_Id(Gtk.List_Store.Get_Int(Win.Table, Row, 0));
         Download_Manager.Pause(Id);
      end if;
   end On_Pause_Button_Pressed;

   ----------------------------
   -- On_Stop_Button_Pressed --
   ----------------------------

   procedure On_Stop_Button_Pressed (Win : access Main_Window_Record'Class) is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;

      Tree : Gtk.Tree_Model.Gtk_Tree_Model;
      Row  : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
      Id   : Download_Manager.Download_Id;
   begin
      Gtk.Tree_Selection.Get_Selected(Gtk.Tree_View.Get_Selection(Win.Table_View), Tree, Row);
      if (Row /= Gtk.Tree_Model.Null_Iter) then
         Id := Download_Manager.Download_Id(Gtk.List_Store.Get_Int(Win.Table, Row, 0));
         Download_Manager.Stop(Id);
      end if;
   end On_Stop_Button_Pressed;

   -----------------------
   -- On_Table_Selected --
   -----------------------

   procedure On_Table_Selected
     (Selection : access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class;  Win : Main_Window_Type) is
   begin
      Echo("On_Table_Selected");
      Update_Buttons(Win);
   end On_Table_Selected;

   ----------------
   -- On_Timeout --
   ----------------

   function On_Timeout (Win : in Main_Window_Type) return Boolean is
   begin
      Update_Tables(Win);
      Update_Buttons(Win);
      return True;
   end On_Timeout;

   -------------------
   -- Update_Tables --
   -------------------

   procedure Update_Tables(Win : in Main_Window_Type) is
      use Download_Manager;
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Ada.Calendar.Time;

      Now : Ada.Calendar.Time := Ada.Calendar.Clock;

      procedure Update_Row(List    : in out Gtk.List_Store.Gtk_List_Store;
                           Row     : in     Gtk.Tree_Model.Gtk_Tree_Iter;
                           Summary : in     Download_Summary) is
         use Ada.Calendar.Formatting;
         use Ada.Calendar.Time_Zones;

         St : Unbounded_String;
      begin
         St :=
           To_Unbounded_String(Image(Summary.Start_Time, Time_Zone => UTC_Time_Offset( Summary.Start_Time ))
                               &
                               " "
                               &

                               Day_Name'Image(Utils.Day_of_Week(Summary.Start_Time, UTC_Time_Offset( Summary.Start_Time ))));

         if Summary.The_State = Sleeping then
            St := St & " (" & Download_Manager.Recurrence'Image(Summary.Frequency) & ")";
         end if;

         Gtk.List_Store.Set (List, Row, 1, To_String(St));
         Gtk.List_Store.Set (List, Row, 2, To_String(Summary.Name));
         Gtk.List_Store.Set (Main_Window.Table, Row, 3, Glib.Gint(Summary.Byte_Count));
         Gtk.List_Store.Set (List, Row, 4, Download_Manager.State'Image(Summary.The_State));
      end Update_Row;

      New_Ids         : Download_Id_Vectors.Vector;
      Id              : Download_Id;
      Summary         : Download_Summary;
      Position        : Download_Summary_Maps.Cursor;
      Changes_Map     : Download_Summary_Maps.Map;
      Downloads_Table : Gtk.List_Store.Gtk_List_Store renames Win.Table;
      X               : Glib.Gint;
      Row             : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
   begin
      Changes_Map := Download_Manager.Get_Last_Changes;
      Row         := Gtk.List_Store.Get_Iter_First (Downloads_Table);
      while Row /= Gtk.Tree_Model.Null_Iter loop
         X  := Gtk.List_Store.Get_Int(Downloads_Table, Row, 0);
         Id := Download_Id(X);
         if  Download_Summary_Maps.Contains(Changes_Map, Id) then
            Summary := Download_Summary_Maps.Element(Changes_Map, Id);
            Update_Row(Downloads_Table, Row, Summary);
            Download_Manager.Download_Summary_Maps.Delete(Changes_Map, Id);
         else
            Download_Id_Vectors.Append(New_Ids, Id);
         end if;

         Gtk.List_Store.Next(Downloads_Table, Row);
      end loop;

      Position := Download_Manager.Download_Summary_Maps.First(Changes_Map);
      while Download_Manager.Download_Summary_Maps.Has_Element(Position) loop
         Summary := Download_Manager.Download_Summary_Maps.Element( Position);
         Gtk.List_Store.Append(Downloads_Table, Row);
         Gtk.List_Store.Set (Downloads_Table, Row, 0, Glib.Gint(Download_Manager.Download_Summary_Maps.Key( Position)));
         Update_Row(Downloads_Table, Row, Summary);
         Download_Manager.Download_Summary_Maps.Next(Position);
      end loop;
   end Update_Tables;

end Main_Window_Pkg.Callbacks;
