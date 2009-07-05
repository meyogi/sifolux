------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Numerics.Discrete_Random;
with Text_IO;
with Ada.Calendar;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Calendar.Formatting;

with GNAT.Calendar.Time_IO;

with Glib.Values;

with Downloaders;
with Config;
with Safe_File;

package body Download_Manager is

   type Status is
      record
         The_State  : State;
         Byte_Count : Natural;
      end record;


   procedure Free is new
     Ada.Unchecked_Deallocation(Downloaders.Downloader,Downloaders.Downloader_Access);
   procedure Free is new
     Ada.Unchecked_Deallocation(Text_IO.File_Type, Text_File_Access);


   ----------------
   -- Is_Working --
   ----------------

   function Is_Working(D : in Download) return Boolean is
   begin
      return (D.The_State = Waiting
              or
                D.The_State = Downloading);
   end Is_Working;

   --------------
   -- Is_Vital --
   --------------

   function Is_Vital(D : in Download) return Boolean is
   begin
      return (D.The_State = Sleeping
              or
                (D.The_State = Paused and D.Is_Live = True)
              or
                D.The_State = Waiting
              or
                D.The_State = Downloading);
   end Is_Vital;

   ----------------------------
   -- Is_Duration_Modifiable --
   ----------------------------

   function Is_Duration_Modifiable(D : in Download) return Boolean is
   begin
      return Is_Vital(D) and then (D.Is_Live or D.The_State = Sleeping);
   end Is_Duration_Modifiable;

   ------------------
   -- Is_Deletable --
   ------------------

   function Is_Deletable(D : in Download) return Boolean is
   begin
      return D.The_State = Sleeping
        or D.The_State = Stopped
        or D.The_State = Finished
        or D.The_State = Failed;
   end Is_Deletable;

   ----------------
   -- Is_Same_Id --
   ----------------

   function Is_Same_Id(Left, Right : Download) return Boolean is
   begin
      return Left.Id = Right.Id;
   end Is_Same_Id;

   -------------
   -- Smaller --
   -------------

   function Smaller(Left, Right : Download) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return (Is_Vital(Left) = Is_Vital(Right) and then Left.Action_Time < Right.Action_Time)
        or else
          (Is_Vital(Left) and not Is_Vital(Right));
   end Smaller;




   package Download_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Download, "=" => Is_Same_Id);

   package Download_Lists_Sorting is
     new Download_Lists.Generic_Sorting("<" => Smaller) ;


   Single_Day        : constant Ada.Calendar.Day_Duration := Ada.Calendar.Day_Duration'Last;
   Single_Week       : constant Duration                  := Single_Day * 7;
   Info_Extension    : constant String                    := "txt";
   Deleted_Info_Mark : constant String                    := "del";
   Next_Id           : Download_Id                        := Download_Id'First;
   Download_List     : Download_Lists.List;


   protected Last_Changes is
      function  Get              return Download_Summary_Maps.Map;
      function  Is_Update_Needed return Boolean;
      procedure Request_Update;
      procedure Set(Changes : Download_Summary_Maps.Map);
   private
      The_Changes   : Download_Summary_Maps.Map;
      Update_Needed : Boolean;
   end Last_Changes;



   protected body Last_Changes is
      function Get return Download_Summary_Maps.Map is
      begin
         -- maybe The_Changes should be cleared
         return The_Changes;
      end Get;

      function Is_Update_Needed return Boolean is
      begin
         return Update_Needed;
      end Is_Update_Needed;

      procedure Request_Update is
      begin
         Update_Needed := True;
      end Request_Update;

      procedure Set(Changes : Download_Summary_Maps.Map) is
      begin
         The_Changes   := Changes;
         Update_Needed := False;
      end Set;
   end Last_Changes;

   --------------------
   -- Assign_Next_Id --
   --------------------

   procedure Assign_Next_Id(D : in out Download) is
   begin
      D.Id    := Next_Id;
      Next_Id := Next_Id + 1;
   end Assign_Next_Id;

   --------------------
   -- Info_Full_Name --
   --------------------

   function Info_Full_Name(D : in Download) return String is
   begin
      return Ada.Directories.Compose(Config.Info_List_Path,
                                     Ada.Strings.Fixed.Trim(Download_Id'Image(D.Id), Ada.Strings.Left) ,
                                     Info_Extension);
   end Info_Full_Name;

   ----------
   -- Save --
   ----------

   procedure Save(D : in Download) is
      File : Text_IO.File_Type;
   begin
      Safe_File.Open(File, Info_Full_Name(D));

      Text_IO.Put_Line(File, Download_Id'Image(D.Id));
      Text_IO.Put_Line(File, To_String(D.Name));
      Text_IO.Put_Line(File, Ada.Calendar.Formatting.Image(D.Requested_Start_Time));
      Text_IO.Put_Line(File, Recurrence'Image(D.Frequency));
      Text_IO.Put_Line(File, Downloaders.Download_Duration'Image(D.Minutes));
      Text_IO.Put_Line(File, To_String(D.Source));
      Text_IO.Put_Line(File, Boolean'Image(D.Is_Live));
      Text_IO.Put_Line(File, State'Image(D.The_State));
      Text_IO.Put_Line(File, To_String(D.Local_Path));
      Text_IO.Put_Line(File, Ada.Streams.Stream_IO.Count'Image(D.Byte_Count));
      Text_IO.Put_Line(File, Ada.Calendar.Formatting.Image(D.Actual_Start_Time));

      Safe_File.Close(File);
   exception
      when E : others =>
         Echo ("Unable to save #" & Download_Id'Image(D.Id));
         Echo (Ada.Exceptions.Exception_Information (E));

         if Safe_File.Is_Open(File) then
            Safe_File.Close_Without_Update(File);
         end if;

   end Save;

   ----------
   -- Load --
   ----------

   procedure Load(Info_File : in Text_IO.File_Type; D : out Download) is
      St   : String(1..125);
      Last : Natural;
   begin

      Text_IO.Get_Line(Info_File, St, Last);
      D.Id                   := Download_Id'Value(Ada.Strings.Fixed.Head(St,Last));

      D.Name                 := To_Unbounded_String(Text_IO.Get_Line(Info_File));

      Text_IO.Get_Line(Info_File, St, Last);
      D.Requested_Start_Time := Ada.Calendar.Formatting.Value(Ada.Strings.Fixed.Head(St,Last));

      Text_IO.Get_Line(Info_File, St, Last);
      D.Frequency            := Recurrence'Value(Ada.Strings.Fixed.Head(St,Last));

      Text_IO.Get_Line(Info_File, St, Last);
      D.Minutes              := Downloaders.Download_Duration'Value(Ada.Strings.Fixed.Head(St,Last));


      D.Source               := To_Unbounded_String(Text_IO.Get_Line(Info_File));

      Text_IO.Get_Line(Info_File, St, Last);
      D.Is_Live              := Boolean'Value(Ada.Strings.Fixed.Head(St,Last));

      Text_IO.Get_Line(Info_File, St, Last);
      D.The_State            := State'Value(Ada.Strings.Fixed.Head(St,Last));

      D.Local_Path           := To_Unbounded_String(Text_IO.Get_Line(Info_File));

      Text_IO.Get_Line(Info_File, St, Last);
      D.Byte_Count           := Ada.Streams.Stream_IO.Count'Value(Ada.Strings.Fixed.Head(St,Last));

      Text_IO.Get_Line(Info_File, St, Last);
      D.Actual_Start_Time    := Ada.Calendar.Formatting.Value(Ada.Strings.Fixed.Head(St,Last));

   end Load;

   -----------------
   -- Delete_Info --
   -----------------
   -- changes the Info file name so that the file will be ignored.

   procedure Delete_Info(D : in Download) is
      use Ada.Directories;
      Original_Full_Name : String := Info_Full_Name(D);
      Deleted_Full_Name  : String :=
        Compose(Containing_Directory(Original_Full_Name),
                Base_Name(Original_Full_Name) & "." & Deleted_Info_Mark,
                Extension(Original_Full_Name));

   begin
      if Exists(Original_Full_Name) then
         Rename(Original_Full_Name, Deleted_Full_Name);
      end if;
   end Delete_Info;

   --------------
   -- Load_All --
   --------------

   function  Load_All return Download_Lists.List is
      File            : Text_IO.File_Type;
      Search          : Ada.Directories.Search_Type;
      Directory_Entry : Ada.Directories.Directory_Entry_Type;
      D               : Download;
      The_List        : Download_Lists.List;
      First_Dot_Index : Natural;
      Id              : Download_Id;
   begin
      Safe_File.Clean(Config.Info_List_Path);
      Ada.Directories.Start_Search(Search,
                                   Config.Info_List_Path,
                                   "*." & Info_Extension,
                                   (False, True, False));
      while Ada.Directories.More_Entries(Search) loop
         Ada.Directories.Get_Next_Entry(Search, Directory_Entry);

         First_Dot_Index := Ada.Strings.Fixed.Index(Ada.Directories.Simple_Name(Directory_Entry), ".");
         Id := Download_Id'Value(
           Ada.Strings.Fixed.Head(
             Ada.Directories.Simple_Name(Directory_Entry),
             First_Dot_Index - 1)

          );

         if Next_Id <= Id then
            Next_Id := Id + 1;
         end if;

         if Ada.Strings.Fixed.Index(Ada.Directories.Simple_Name(Directory_Entry),
                                    Deleted_Info_Mark) = 0 then
            begin
               Text_IO.Open(File, Text_IO.In_File, Ada.Directories.Full_Name(Directory_Entry));
               Init(D);
               Load(File, D);
               Download_Lists.Append(The_List, D);
            exception
               when E : others =>
                  Echo ("Unable to load #" & Download_Id'Image(Id));
                  Echo (Ada.Exceptions.Exception_Information (E));
            end;
            if Text_IO.Is_Open(File) then
               Text_IO.Close(File);
            end if;
         end if;
      end loop;

      Ada.Directories.End_Search(Search);
      return The_List;
   end Load_All;

   ------------------------
   -- Estimated_End_Time --
   ------------------------

   function Estimated_End_Time(D : in Download) return Ada.Calendar.Time is
      use type Ada.Calendar.Time;
   begin
      return  D.Requested_Start_Time + Duration(D.Minutes * 60);
   end Estimated_End_Time;

   -----------------------
   -- Remove_Downloader --
   -----------------------

   procedure Remove_Downloader(D : in out Download) is
      use type Downloaders.Downloader_Access;
   begin
      if D.P_Downloader /= null then
         Downloaders.Quit(D.P_Downloader);
         Free(D.P_Downloader);
         D.P_Downloader := null;
      end if;
   end Remove_Downloader;

   ----------
   -- Init --
   ----------

   procedure Init is
      procedure Create_Downloader(D : in out Download) is
      begin
         Downloaders.Create(Ref                => D.P_Downloader,
                            Source             => To_String(D.Source),
                            Local_Path         => D.Local_Path,
                            Estimated_Duration => D.Minutes,
                            Is_Live            => D.Is_Live);
      end Create_Downloader;

      D        : Download;
      St       : Unbounded_String;
      The_List : Download_Lists.List;
      Position : Download_Lists.Cursor;
   begin

      The_List := Load_All;
      Position := Download_Lists.First(The_List);

      while Download_Lists.Has_Element(Position) loop
         begin
            D        := Download_Lists.Element(Position);
            D.Is_New := False;
            if D.The_State = Downloading then
               D.The_State := Waiting;
            end if;

            if D.The_State = Waiting then
               D.Action_Time := Estimated_End_Time(D);
               Create_Downloader(D);
               Downloaders.Start(D.P_Downloader);

            elsif D.The_State = Paused then
               Create_Downloader(D);

               if D.Is_Live then
                  Downloaders.Stop(D.P_Downloader);
                  Remove_Downloader(D);
                  D.The_State := Stopped;
               end if;
            end if;

            Save(D);
            Download_List.Append(D);
         exception
            when E : others =>
               Echo ("Download_Manager: Init: " & Ada.Exceptions.Exception_Information (E));
               Remove_Downloader(D);
         end;

         Download_Lists.Next(Position);
      end loop;
   end Init;

   ----------------
   -- Get_Cursor --
   ----------------

   function Get_Cursor(Id : in Download_Id) return Download_Lists.Cursor is
      Current_Cursor : Download_Lists.Cursor;
      D              : Download;
      Is_Found       : Boolean  := False;
   begin
      Current_Cursor := Download_Lists.First(Download_List);
      while (not Is_Found) and then Download_Lists.Has_Element(Current_Cursor) loop
         D :=  Download_Lists.Element(Current_Cursor);
         if D.Id = Id then
            Is_Found := True;
         else
            Download_Lists.Next(Current_Cursor);
         end if;

      end loop;

      return Current_Cursor;
   end Get_Cursor;

   ---------------------
   -- Next_Start_Time --
   ---------------------

   function Next_Start_Time(D : in Download; Since : in Ada.Calendar.Time)
                            return Ada.Calendar.Time is
      use type Ada.Calendar.Time;

      Ret_Val : Ada.Calendar.Time := Since;
      Period  : Duration          := Single_Week * 520;
   begin
      if Since <= D.Requested_Start_Time then
         Ret_Val := D.Requested_Start_Time;

      elsif D.Frequency = Once then
         Ret_Val := D.Requested_Start_Time;
      else
         case D.Frequency is
            when Every_Day  => Period := Single_Day;
            when Every_Week => Period := Single_Week;
            when others     => Period := Single_Week * 1040;
         end case;
         Ret_Val := D.Requested_Start_Time + ( Integer((Since - D.Requested_Start_Time) / Period) + 1) * Period;
      end if;

      return Ret_Val;
   end Next_Start_Time;

   -------------------------
   -- Summary_Of_Download --
   -------------------------

   function Summary_Of_Download(D : in Download) return Download_Summary is
      S : Download_Summary;
   begin
      S := (Id         => D.Id,
            Name       => D.Name,
            Byte_Count => D.Byte_Count,
            Start_Time => D.Requested_Start_Time,
            Frequency  => D.Frequency,
            The_State  => D.The_State);

      return S;
   end Summary_Of_Download;

   ------------------------
   -- Compose_Local_Path --
   ------------------------

   function Compose_Local_Path(D : in Download) return Unbounded_String is
      Underscore_Index : Natural;
      Search           : Ada.Directories.Search_Type;
      Directory_Entry  : Ada.Directories.Directory_Entry_Type;
      Path             : Unbounded_String;
      Index_String     : Unbounded_String;
      Date_String      : String   := GNAT.Calendar.Time_IO.Image(D.Actual_Start_Time,"%Y%m%d");
      Available_Index  : Natural  := 0;
      Index            : Natural  := 0;
   begin
      Path := To_Unbounded_String(Ada.Directories.Compose(Config.Base_Local_Path, To_String(D.Name)));
      if Ada.Directories.Exists(To_String(Path)) then

      Ada.Directories.Start_Search(Search, To_String(Path) , Date_String & "*" , (True, False, False));
      while Ada.Directories.More_Entries(Search) loop
         Ada.Directories.Get_Next_Entry(Search, Directory_Entry);
          declare
            Dir_Name : String := Ada.Directories.Simple_Name(Directory_Entry);
         begin
            Underscore_Index := Ada.Strings.Fixed.Index(Dir_Name, "_");
            if Underscore_Index > 0 then
               Index := Natural'Value(Ada.Strings.Fixed.Tail(Dir_Name, Dir_Name'Length - Underscore_Index ));
            else
               Index := 0;
            end if;

            if Index >= Available_Index then
               Available_Index := Index + 1;
            end if;
         end;
      end loop;
         Ada.Directories.End_Search(Search);
      else
         Available_Index := 0;
      end if;


      if Available_Index = 0 then
         Index_String := Null_Unbounded_String;
      else
         Index_String :=
           To_Unbounded_String("_" & Ada.Strings.Fixed.Trim(Natural'Image(Available_Index),Ada.Strings.Left));
      end if;

      Path := To_Unbounded_String(Ada.Directories.Compose(To_String(Path),To_String( Date_String & Index_String )));
      return Path;
   end Compose_Local_Path;

   --------------
   -- Destruct --
   --------------

   procedure Destruct(D : in out Download) is
   begin
      Remove_Downloader(D);
   end Destruct;

   ----------------------
   -- Stop_Downloading --
   ----------------------

   procedure Stop_Downloading is
      Position : Download_Lists.Cursor;
      D        : Download;
   begin
      Position := Download_Lists.First(Download_List);

      while Download_Lists.Has_Element(Position) loop
         D := Download_Lists.Element(Position);
         Save(D);
         Destruct(D);
         Download_Lists.Replace_Element(Download_List, Position, D);
         Download_Lists.Next(Position);
      end loop;

   end Stop_Downloading;

   -----------------
   -- task Worker --
   -----------------

   task Worker is
      entry Get_Download(Id : in Download_Id; D : out Download);
      entry Exists(Id : in Download_Id; Result: out Boolean);
      entry Modify(Id : in Download_Id; D : in Download);
      entry Add(D : in Download);
      entry Delete(Id : in Download_Id);
      entry Pause(Id : in Download_Id);
      entry Stop(Id : in Download_Id);
      entry Start(Id : in Download_Id);
      entry Wake_Up;
      entry Quit;
   end Worker;


   task body Worker is
      use type Ada.Streams.Stream_IO.Count;
      use type Ada.Calendar.Time;

      Current_Summaries : Download_Summary_Maps.Map;

      procedure Change_Download(Position : in Download_Lists.Cursor; D : in Download) is
         D2 : Download := D;
      begin
         D2.Is_Changed := True;
         Download_Lists.Replace_Element(Download_List, Position, D2);
         Download_Summary_Maps.Include(Current_Summaries,  D2.Id, Summary_Of_Download(D2));
      end Change_Download;

      Prev_State        : State;
      Position          : Download_Lists.Cursor;
      Current_Position  : Download_Lists.Cursor;
      Now               : Ada.Calendar.Time;
      Next_Action_Time  : Ada.Calendar.Time;
      Current_Download  : aliased   Download;
      D1                : aliased   Download;
      P_Download        : access Download;
      The_Status        : Downloaders.Status;
      Is_More           : Boolean;
      Is_Enough         : Boolean                   := False;
      Currently_Working : Boolean                   := True;
      Waiting_Duration  : Ada.Calendar.Day_Duration := 0.5;
      Is_Sort_Needed    : Boolean                   := False;
   begin
      select
         accept Wake_Up do
            Init;
         end Wake_Up;
      or
         accept Quit do
            Is_Enough := True;
         end Quit;
      end select;

      while not Is_Enough loop
         Now              := Ada.Calendar.Clock;
         Next_Action_Time := Now + 0.5;
         Current_Position := Download_Lists.First(Download_List);
         Is_More          := Download_Lists.Has_Element(Current_Position);
         while Is_More loop
            Current_Download := Download_Lists.Element(Current_Position);
            if Is_Vital( Current_Download) then
               if Current_Download.The_State = Sleeping then
                  P_Download := Current_Download'Access;
                  Position   := Current_Position;
                  if Current_Download.Frequency /= Once and Current_Download.Requested_Start_Time - 120.0 <= Now then
                     D1         := Current_Download;
                     P_Download := D1'Access;
                     Assign_Next_Id(P_Download.all);
                     P_Download.Frequency := Once;
                     Save(P_Download.all);
                     Current_Download.Requested_Start_Time := Next_Start_Time(Current_Download, Current_Download.Requested_Start_Time + 0.1);
                     Change_Download(Current_Position, Current_Download);
                     Save(Current_Download);
                     Download_Lists.Prepend (Download_List, P_Download.all);
                     Position := Download_Lists.First(Download_List);
                  end if;

                  if P_Download.Requested_Start_Time - 60.0 <= Now then
                     if (Estimated_End_Time(P_Download.all) <= Now) and then
                       (
                        P_Download.Is_Live
                        or
                          Estimated_End_Time(P_Download.all) > Next_Start_Time(P_Download.all, Now)
                       ) then
                        P_Download.The_State := Missed;
                     else
                        begin
                           P_Download.The_State         := Waiting;
                           P_Download.Action_Time       := Estimated_End_Time(P_Download.all);
                           P_Download.Actual_Start_Time := Now;
                           P_Download.Local_Path        := Compose_Local_Path(P_Download.all);
                           Downloaders.Create(Ref                => P_Download.P_Downloader,
                                              Source             => To_String(P_Download.Source),
                                              Local_Path         => P_Download.Local_Path,
                                              Estimated_Duration => P_Download.Minutes,
                                              Is_Live            => P_Download.Is_Live);
                           Currently_Working := True;
                           Downloaders.Start(P_Download.P_Downloader);
                        exception
                           when E : others =>
                           Echo("Download_Manager: Worker:");
                              Echo (Ada.Exceptions.Exception_Information (E));
                              P_Download.The_State := Failed;
                              Remove_Downloader(P_Download.all);
                        end;

                     end if;
                     Change_Download(Position, P_Download.all);
                     Save(P_Download.all);
                  end if; -- Requested_Start_Time - 60 <= Now

               elsif Current_Download.Is_Live and Estimated_End_Time(Current_Download) <= Now then
                  Remove_Downloader(Current_Download);
                  Current_Download.The_State := Finished;
                  Change_Download(Current_Position, Current_Download);
                  Save(Current_Download);
               elsif Current_Download.The_State = Waiting then
                  null;
               else
                  null;
               end if; -- Current_Download.The_State = ???

               Download_Lists.Next(Current_Position);
               Is_More := Download_Lists.Has_Element(Current_Position);
            else -- not vital
               Is_More := False;
            end if;
         end loop; -- while Is_More

         Download_Lists_Sorting.Sort(Download_List);
         Current_Position := Download_Lists.First(Download_List);
         if Download_Lists.Has_Element(Current_Position) then
            Current_Download := Download_Lists.Element(Current_Position);
            if Is_Vital(Current_Download) then
               if Currently_Working then
                  Next_Action_Time := Now + 0.9;
               else
                  Next_Action_Time := Current_Download.Action_Time;
               end if;
            end if;
         end if;

         if Last_Changes.Is_Update_Needed then
            Currently_Working := False;
            Position          := Download_Lists.First(Download_List);
            while Download_Lists.Has_Element(Position) loop
               D1 := Download_Lists.Element(Position);
               if Is_Vital(D1) and D1.The_State /= Sleeping  then
                  The_Status := Downloaders.Get_Status(D1.P_Downloader);
                  if The_Status.Byte_Count > 0 then
                     D1.Byte_Count := The_Status.Byte_Count;
                  end if;

                  Prev_State := D1.The_State;
                  case The_Status.The_State is
                     when Downloaders.Waiting =>
                        D1.The_State := Waiting;
                     when Downloaders.Downloading =>
                        D1.The_State := Downloading;
                     when Downloaders.Paused =>
                        D1.The_State := Paused;
                     when Downloaders.Stopped =>
                        D1.The_State := Stopped;
                     when Downloaders.Sleeping =>
                        null;
                     when Downloaders.Finished =>
                        D1.The_State := Finished;
                     when Downloaders.Failed =>
                        D1.The_State := Failed;
                  end case;

                  if D1.The_State /= Prev_State and
                    (not ((D1.The_State = Waiting and Prev_State = Downloading)
                          or (D1.The_State = Downloading and Prev_State = Waiting))) then
                     Save(D1);
                  end if;

                  if Is_Working(D1) then
                     Currently_Working := True;
                  end if;
                  D1.Is_Changed  := True;
                  Is_Sort_Needed := True;
               end if;

               if D1.Is_Changed then
                  D1.Is_Changed := False;
                  Download_Lists.Replace_Element(Download_List, Position, D1);
                  Download_Summary_Maps.Include(Current_Summaries,  D1.Id, Summary_Of_Download(D1));
               end if;

               Position := Download_Lists.Next(Position);
            end loop;
            Last_Changes.Set(Current_Summaries);
            Download_Summary_Maps.Clear(Current_Summaries);
            if Is_Sort_Needed then
               Download_Lists_Sorting.Sort(Download_List);
               Is_Sort_Needed := False;
            end if;

         end if;

         select
            accept Get_Download(Id : in Download_Id; D : out Download) do
               D := Download_Lists.Element(Get_Cursor(Id));
            end Get_Download;
         or
            accept Exists(Id : in Download_Id; Result: out Boolean) do
               Result := Download_Lists.Has_Element(Get_Cursor(Id));
            end Exists;
         or
            accept Modify(Id : in Download_Id; D : in Download) do
               Position := Get_Cursor(Id);
               D1 :=  Download_Lists.Element( Position);
               if D1.The_State = Sleeping then
                  D1.Name                 := D.Name;
                  D1.Source               := D.Source;
                  D1.Requested_Start_Time := D.Requested_Start_Time;
                  D1.Actual_Start_Time    := D1.Requested_Start_Time;
                  D1.Frequency            := D.Frequency;
                  D1.Is_Live              := D.Is_Live;
               end if;

               if Is_Duration_Modifiable(D1) then
                  D1.Minutes := D.Minutes;
               end if;

               if Is_Vital(D1) then
                  if D1.The_State = Sleeping then
                     D1.Action_Time := D1.Requested_Start_Time;
                  else
                     D1.Action_Time := Estimated_End_Time(D1);
                  end if;
               end if;

               Change_Download(Position, D1);
               Last_Changes.Set(Current_Summaries);
               Save(D1);
               Download_Lists_Sorting.Sort(Download_List);
               Next_Action_Time := Now;

            exception
               when E : others =>
                  Echo ("Unable to Modify #" & Download_Id'Image(Id));
                  Echo (Ada.Exceptions.Exception_Information (E));
            end Modify;
         or
            accept Add(D : in Download) do
               D1 := D;
               Assign_Next_Id(D1);
               D1.Is_New            := False;
               D1.The_State         := Sleeping;
               D1.Action_Time       := D1.Requested_Start_Time - 60.0;
               D1.Actual_Start_Time := D1.Requested_Start_Time;
               Save(D1);
               Download_Lists.Append (Download_List, D1);
               Download_Lists_Sorting.Sort(Download_List);
               Download_Summary_Maps.Include(Current_Summaries, D1.Id, Summary_Of_Download(D1));
               Last_Changes.Set(Current_Summaries);
               Next_Action_Time := Now;
            end Add;
         or
            accept Delete(Id : in Download_Id) do
               Position := Get_Cursor(Id);
               if Download_Lists.Has_Element(Position) then
                  D1 :=  Download_Lists.Element( Position);
                  if Is_Deletable(D1) then
                     Download_Summary_Maps.Exclude(Current_Summaries, D1.Id);
                     Last_Changes.Set(Current_Summaries);
                     Download_Lists.Delete(Download_List, Position);
                     Delete_Info(D1);
                  end if;
               end if;
            end Delete;
         or
            accept Pause(Id : in Download_Id) do
               Position := Get_Cursor(Id);
               D1       :=  Download_Lists.Element( Position);
               if Is_Working(D1) then
                  Downloaders.Pause(D1.P_Downloader);
                  Change_Download(Position, D1);
                  Download_Lists_Sorting.Sort(Download_List);
               end if;

            exception
               when E : others =>
                  Echo ("Unable to Pause #" & Download_Id'Image(Id));
                  Echo (Ada.Exceptions.Exception_Information (E));
            end Pause;
         or
            accept Stop(Id : in Download_Id) do
               Position := Get_Cursor(Id);
               D1       :=  Download_Lists.Element( Position);
               if D1.Is_Live and then (Is_Working(D1) or D1.The_State = Paused) then
                  Downloaders.Stop(D1.P_Downloader);
                  Change_Download(Position, D1);
                  Download_Lists_Sorting.Sort(Download_List);
               end if;

            exception
               when E : others =>
                  Echo ("Unable to Stop #" & Download_Id'Image(Id));
                  Echo (Ada.Exceptions.Exception_Information (E));
            end Stop;
         or
            accept Start(Id : in Download_Id) do
               Position := Get_Cursor(Id);
               D1       :=  Download_Lists.Element( Position);
               if D1.The_State = Paused then
                  Downloaders.Start(D1.P_Downloader);
               end if;

            exception
               when E : others =>
                  Echo ("Unable to Start #" & Download_Id'Image(Id));
                  Echo (Ada.Exceptions.Exception_Information (E));
            end Start;
         or
            accept Quit do
               Is_Enough := True;
            end Quit;
         or
            delay until Next_Action_Time;
         end select;

      end loop;

      Stop_Downloading;

   exception
      when E : others =>
         Echo ("Download_Manager: Worker: " & Ada.Exceptions.Exception_Information (E));
   end Worker;

   ----------
   -- Quit --
   ----------

   procedure Quit is
   begin
      Worker.Quit;
   exception
      when TASKING_ERROR =>
         Echo("Task already stopped");
   end Quit;

   ---------
   -- Get --
   ---------

   function Get(Id : in Download_Id) return Download is
      D : Download;
   begin
      Worker.Get_Download(Id, D);
      return D;
   end Get;

   ------------
   -- Exists --
   ------------

   function Exists(Id : in Download_Id) return Boolean is
      Ret_Val : Boolean;
   begin
      Worker.Exists(Id, Ret_Val);
      return Ret_Val;
   end Exists;

   ----------
   -- Init --
   ----------

   procedure Init(New_Download : out Download) is
   begin
      New_Download.Is_New               := True;
      New_Download.Name                 := To_Unbounded_String("default");
      New_Download.Source               := To_Unbounded_String("");
      New_Download.Requested_Start_Time := Ada.Calendar.Clock;
      New_Download.Frequency            := Once;
      New_Download.Minutes              := 120;
      New_Download.Is_Live              := True;
      New_Download.The_State            := Sleeping;
      New_Download.Action_Time          := New_Download.Requested_Start_Time;
      New_Download.P_Downloader         := null;
   end Init;

   ---------------
   -- Init_Like --
   ---------------

   procedure Init_Like(New_Download : out Download; Old_Download : in Download) is
   begin
      New_Download                      := Old_Download;
      New_Download.Is_New               := True;
      New_Download.Byte_Count           := 0;
      New_Download.Requested_Start_Time := Ada.Calendar.Clock;
      New_Download.Frequency            := Once;
      New_Download.The_State            := Sleeping;
      New_Download.Action_Time          := New_Download.Requested_Start_Time;
      New_Download.P_Downloader         := null;
   end Init_Like;

   ------------
   -- Modify --
   ------------

   procedure Modify(Id : in Download_Id; D : in Download) is
   begin
      Worker.Modify(Id, D);
   end Modify;

   ---------
   -- Add --
   ---------

   procedure Add(D : in Download) is
   begin
      Worker.Add(D);
   end Add;

   ------------
   -- Delete --
   ------------

   procedure Delete(Id : in Download_Id) is
   begin
      Worker.Delete(Id);
   end Delete;

   -----------
   -- Pause --
   -----------

   procedure Pause(Id : in Download_Id) is
   begin
      Worker.Pause(Id);
   end Pause;

   ----------
   -- Stop --
   ----------

   procedure Stop(Id : in Download_Id) is
   begin
      Worker.Stop(Id);
   end Stop;

   -----------
   -- Start --
   -----------

   procedure Start(Id : in Download_Id) is
   begin
      Worker.Start(Id);
   end Start;

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      Worker.Wake_Up;
   end Run;

   ----------------------
   -- Get_Last_Changes --
   ----------------------

   function Get_Last_Changes return Download_Summary_Maps.Map is
      Summaries : Download_Summary_Maps.Map;
   begin
      Summaries := Last_Changes.Get;
      Last_Changes.Request_Update;
      return Summaries;
   end Get_Last_Changes;

   --------------
   -- Get_Name --
   --------------

   function Get_Name(D : in Download) return String is
   begin
      return To_String(D.Name);
   end Get_Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name(D : in out Download; Name : in String) is
   begin
      D.name := To_Unbounded_String(Name);
   end Set_Name;

   ------------
   -- Get_Id --
   ------------

   function Get_Id(D : in Download) return Download_Id is
   begin
      return D.Id;
   end Get_Id;

   ------------------------------
   -- Get_Requested_Start_Time --
   ------------------------------

   function Get_Requested_Start_Time(D : in Download) return Ada.Calendar.Time is
   begin
      return D.Requested_Start_Time;
   end Get_Requested_Start_Time;

   ------------------------------
   -- Set_Requested_Start_Time --
   ------------------------------

   procedure Set_Requested_Start_Time(D : in out Download; Start_Time : in Ada.Calendar.Time) is
   begin
      D.Requested_Start_Time := Start_Time;
   end Set_Requested_Start_Time;

   -------------------
   -- Get_Frequency --
   -------------------

   function Get_Frequency(D : in Download) return Recurrence is
   begin
      return D.Frequency;
   end Get_Frequency;

   -------------------
   -- Set_Frequency --
   -------------------

   procedure Set_Frequency(D : in out Download; Frequency : in Recurrence) is
   begin
      D.Frequency := Frequency;
   end Set_Frequency;

   -----------------
   -- Get_Minutes --
   -----------------

   function Get_Minutes(D : in Download) return Downloaders.Download_Duration is
   begin
      return D.Minutes;
   end Get_Minutes;

   -----------------
   -- Set_Minutes --
   -----------------

   procedure Set_Minutes(D : in out Download; Minutes : in Downloaders.Download_Duration) is
   begin
      D.Minutes := Minutes;
   end Set_Minutes;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address(D : in Download) return String is
   begin
      return To_String(D.Source);
   end Get_Address;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address(D : in out Download; Source : in String) is
   begin
      D.Source := To_Unbounded_String(Source);
   end Set_Address;

   -------------
   -- Is_Live --
   -------------

   function Is_Live(D : in Download) return Boolean is
   begin
      return D.Is_Live;
   end Is_Live;

   --------------
   -- Set_Live --
   --------------

   procedure Set_Live(D : in out Download; Is_Live : in Boolean) is
   begin
      D.Is_Live := Is_Live;
   end Set_Live;

   ---------------
   -- Get_State --
   ---------------

   function Get_State(D : in Download) return State is
   begin
      return D.The_State;
   end Get_State;

   ------------
   -- Is_New --
   ------------

   function Is_New(D : in Download) return Boolean is
   begin
      return D.Is_New;
   end Is_New;

   ------------------
   -- Is_Url_Valid --
   ------------------

   function Is_Url_Valid(Address : in String) return Boolean is
   begin
      return Ada.Strings.Fixed.Index(Ada.Strings.Fixed.Trim(Address, Ada.Strings.Both), ".") > 6;
   end Is_Url_Valid;

   ---------------------------
   -- Is_Protocol_Supported --
   ---------------------------

   function Is_Protocol_Supported(Address : in String) return Boolean is
   begin
      return Ada.Strings.Fixed.Index(Ada.Strings.Fixed.Trim(Address, Ada.Strings.Both), "mms://") = 1;
   end Is_Protocol_Supported;

end Download_Manager;
