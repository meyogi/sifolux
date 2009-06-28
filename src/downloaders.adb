------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Text_IO;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Exceptions;
with Ada.Calendar;
with Ada.Numerics.Discrete_Random;

with Utils;                 use Utils;
with Protocols;
with Top_Protocols;





package body Downloaders is

   Tmp_Dir           : constant String := "tmp";
   Done_Dir          : constant String := "tmp_done";
   File_Name_Pattern : constant String := "__*_*";

   package Random_Positive is new Ada.Numerics.Discrete_Random (Positive);

   procedure Free is new
     Ada.Unchecked_Deallocation(Top_Protocols.Account'Class, Top_Protocols.Account_Class_Access);

   ----------------------
   -- Protected_Status --
   ----------------------

   protected body Protected_Status is
      function Get return Status is
      begin
         return Current_Status;
      end Get;

      procedure Set(New_Status : Status) is
      begin
         Current_Status := New_Status;
      end Set;

      procedure Set_State(New_State : State) is
      begin
         Current_Status.The_State := New_State;
      end Set_State;

   end Protected_Status;

   --------------------
   -- Remove_Account --
   --------------------

   procedure Remove_Account(This : access Downloader) is
      use type Top_Protocols.Account_Class_Access;
   begin
      if This.The_Account /= null then
         Free(This.The_Account);
         This.The_Account := null;
      end if;
   end Remove_Account;

   -----------------
   -- Flatten_Dir --
   -----------------

   procedure Flatten_Dir(Path : in String) is
      use Ada.Directories;
      Search           : Ada.Directories.Search_Type;
      Directory_Entry  : Ada.Directories.Directory_Entry_Type;
      Containing_Path  : constant String := Containing_Directory(Path);
   begin
      Echo("Flatten_Dir (" & Path & ")");
      Start_Search(Search, Path, "*",(False, True, False));
      while More_Entries(Search) loop
         Get_Next_Entry(Search, Directory_Entry);
         declare
            Name          : String := Simple_Name(Directory_Entry);
            Old_Full_Name : String := Full_Name(Directory_Entry);
            New_Full_Name : String := Compose(Containing_Path, Name);
         begin
            if Exists(New_Full_Name) then
               -- just in case ...
               declare
                  G   : Random_Positive.Generator;
                  Num : Positive;
               begin
                  Random_Positive.Reset(G);
                  Num := Random_Positive.Random(G);
                  Rename(New_Full_Name, Compose(Containing_Path,
                    "BACKUP_" & Ada.Strings.Fixed.Trim(Positive'Image(Num),Ada.Strings.Left)) & "_" & Name);
               end;

            end if;
            Rename(Old_Full_Name, New_Full_Name);
         end;
      end loop;
      Ada.Directories.End_Search(Search);
      Delete_Directory(Path);
   end Flatten_Dir;

   -------------------
   -- task Receiver --
   -------------------

   task body Receiver is
      use type Ada.Calendar.Time;

      File        : Ada.Streams.Stream_IO.File_Type;
      The_Account : Top_Protocols.Account_Class_Access;
      Last_Save   : Ada.Calendar.Time;
      Now         : Ada.Calendar.Time;
      Is_Enough   : Boolean                     := False;
      Is_Running  : Boolean                     := False;
      Byte_Count  : Ada.Streams.Stream_IO.Count := 0;
      Signature0  : constant String             := "Downloadres: Receiver";
      Signature   : Unbounded_String            := To_Unbounded_String(Signature0);
   begin
      while not Is_Enough loop
         Now := Ada.Calendar.Clock;
         select
            accept Start(Account : in Top_Protocols.Account_Class_Access; Full_Name : in String) do
               Signature := To_Unbounded_String(Signature0 & " (" & Full_Name & ")");
               Echo(To_String(Signature) & " : Start");
               The_Account := Account;
               if Ada.Streams.Stream_IO.Is_Open(File) then
                  Is_Running := False;
                  Ada.Streams.Stream_IO.Close(File);
               end if;
               Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.Append_File, Full_Name);
               Byte_Count := 0;
               Is_Running := True;
               Last_Save  := Now;
            end Start;
         or
            accept Stop do
               if Ada.Streams.Stream_IO.Is_Open(File) then
                  Ada.Streams.Stream_IO.Close(File);
                  Is_Running := False;
               end if;
            end Stop;
         or
            accept Get_Byte_Count(Count : out Ada.Streams.Stream_IO.Count) do
               Count := Byte_Count;
            end Get_Byte_Count;
         or
            accept Get_Is_Running(Running : out Boolean) do
               Running := Is_Running;
            end Get_Is_Running;
         or
            accept Quit do
               if Ada.Streams.Stream_IO.Is_Open(File) then
                  Is_Running := False;
                  Ada.Streams.Stream_IO.Close(File);
               end if;
               Is_Enough := True;
            end Quit;
         else
            null;
         end select;

         if Is_Running then
            declare
               use type Ada.Streams.Stream_IO.Count;
               use type Ada.Streams.Stream_Element_Offset;

               Total : Ada.Streams.Stream_Element_Count;
               Bytes : Ada.Streams.Stream_Element_Array(1..Top_Protocols.Get_Packet_Len(The_Account.all));

            begin
               Total := 0;
               select
                  delay 25.0;
               then abort
                  Top_Protocols.Read(The_Account.all, Bytes, Total);
               end select;
               if Total > 0 then
                  Ada.Streams.Stream_IO.Write(File, Bytes(1..Total));
               else
                  Echo(To_String(Signature) & " : timeout");
                  Ada.Streams.Stream_IO.Close(File);
                  Is_Running := False;
               end if;
               Byte_Count := Byte_Count + Ada.Streams.Stream_IO.Count(Total);
            end;
            if Now - Last_Save > 10.0 then
               Ada.Streams.Stream_IO.Flush(File);
               Last_Save := Now;
            end if;
         end if;
      end loop;
   exception
      when E : others =>
         Echo(To_String(Signature) & " : exception : " & Ada.Exceptions.Exception_Information (E));
         if Ada.Streams.Stream_IO.Is_Open(File) then
            Ada.Streams.Stream_IO.Close(File);
         end if;
   end Receiver;

   ----------------------
   -- Compose_Tmp_Path --
   ----------------------

   function Compose_Tmp_Path(This : access Downloader)  return String is
   begin
      return Ada.Directories.Compose(To_String(This.Local_Path), Tmp_Dir);
   end Compose_Tmp_Path;

   -----------------------
   -- Compose_Done_Path --
   -----------------------

   function Compose_Done_Path(This : access Downloader)  return String is
   begin
      return Ada.Directories.Compose(To_String(This.Local_Path), Done_Dir);
   end Compose_Done_Path;

   ---------------
   -- Finish_Up --
   ---------------

   procedure Finish_Up(This : access Downloader) is
      Tmp_Path  : String := Compose_Tmp_Path(This);
      Done_Path : String := Compose_Done_Path(This);
   begin
      Echo("Finish_Up (" & Tmp_Path & ")");
      if Ada.Directories.Exists(Tmp_Path) then
         Ada.Directories.Rename(Tmp_Path, Done_Path);
      end if;
      if Ada.Directories.Exists(Done_Path) then
         Flatten_Dir(Done_Path);
      end if;
   end Finish_Up;

   ---------------------------
   -- Next_Output_Full_Name --
   ---------------------------

   function Next_Output_Full_Name(Path          : in String;
                                  Original_Name : in String) return String is
      Search                : Ada.Directories.Search_Type;
      Directory_Entry       : Ada.Directories.Directory_Entry_Type;
      After_Number_Position : Positive;
      Full_Name             : Unbounded_String;
      Number_Position       : constant Positive := Ada.Strings.Fixed.Index(File_Name_Pattern, "*");
      Index                 : Natural           := 0;
      Available_Index       : Natural           := 0;
   begin
      Ada.Directories.Start_Search(Search, Path, File_Name_Pattern, (False, True, False));
      while Ada.Directories.More_Entries(Search) loop
         Ada.Directories.Get_Next_Entry(Search, Directory_Entry);
         declare
            Simple_Name : constant String := Ada.Directories.Simple_Name(Directory_Entry);
         begin
            After_Number_Position := Ada.Strings.Fixed.Index(Simple_Name, "_", Number_Position);
            if After_Number_Position > Number_Position then
               begin
                  Index := Positive'Value(Slice(To_Unbounded_String(Simple_Name), Number_Position, After_Number_Position - 1));
                  if Index >= Available_Index then
                     Available_Index := Index + 1;
                  end if;
               exception
                  when others =>
                     Echo("Next_Output_Full_Name: Exception while finding out target file name");
               end;
            end if;
         end;
      end loop;
      Ada.Directories.End_Search(Search);

      if Index = 0 then
         if Ada.Directories.Exists(Ada.Directories.Compose(Path, Original_Name)) then
            Available_Index := 1;
         else
            Available_Index := 0;
         end if;
      end if;
      if Available_Index = 0 then
         Full_Name := To_Unbounded_String(Ada.Directories.Compose(Path, Original_Name));
      else
         Full_Name := To_Unbounded_String(Ada.Directories.Compose(Path,
           To_String((Number_Position - 1) * "_" )
           & Ada.Strings.Fixed.Trim(Positive'Image(Available_Index),Ada.Strings.Left)
           & "_"
           & Original_Name));
      end if;
      return To_String(Full_Name);
   end Next_Output_Full_Name;

   ---------------------
   -- task Controller --
   ---------------------

   task body Controller is
      use type Top_Protocols.Account_Class_Access;
      use type Ada.Streams.Stream_IO.Count;
      use type Ada.Calendar.Time;

      function Signature return String is
      begin
         return "Downloaders: Controller (" & To_String(Ref.Local_Path) & ")";
      end Signature;


      Current_Status          : Downloaders.Status;
      Actual_State            : Downloaders.State;
      Next_State              : Downloaders.State;
      New_State               : Downloaders.State;
      Tmp_Path                : Unbounded_String;
      Done_Path               : Unbounded_String;
      File_Name               : Unbounded_String;
      Output_Full_Name        : Unbounded_String;
      Output_File             : Ada.Streams.Stream_IO.File_Type;
      Idle_Since              : Ada.Calendar.Time;
      Now                     : Ada.Calendar.Time;
      Last_Connection_Attempt : Ada.Calendar.Time;
      Waiting_Duration        : Ada.Calendar.Day_Duration   := 0.01;
      Saved_Byte_Count        : Ada.Streams.Stream_IO.Count := 0;
      Current_Byte_Count      : Ada.Streams.Stream_IO.Count := 0;
      Old_Byte_Count          : Ada.Streams.Stream_IO.Count := 0;
      New_Byte_Count          : Ada.Streams.Stream_IO.Count := 0;
      Byte_Count              : Natural                     := 0;
      Is_Enough               : Boolean                     := False;
      Is_Running              : Boolean                     := False;
   begin
      select
         accept Wake_Up do
            Echo(Signature);
            Tmp_Path  := To_Unbounded_String(Compose_Tmp_Path(Ref));
            Done_Path := To_Unbounded_String(Compose_Done_Path(Ref));
            if Ada.Directories.Exists(To_String(Ref.Local_Path)) then
               if not Ada.Directories.Exists(To_String(Tmp_Path)) then
                  if Ada.Directories.Exists(To_String(Done_Path)) then
                     Flatten_Dir(To_String(Done_Path));
                  end if;
                  if Ref.Is_Live then
                     Ref.The_Status.Set_State(Stopped);
                  else
                     Ref.The_Status.Set_State(Finished);
                  end if;
               end if;
            else
               Ada.Directories.Create_Path(To_String(Tmp_Path));
            end if;
         end Wake_Up;
      end select;

      Waiting_Duration        := 0.0;
      Idle_Since              := Ada.Calendar.Clock;
      Last_Connection_Attempt := Ada.Calendar.Clock - 1000.0;

      while not Is_Enough loop
         Now            := Ada.Calendar.Clock;
         Current_Status := Ref.The_Status.Get;
         Actual_State   := Current_Status.The_State;

         if   Actual_State = Finished
           or Actual_State = Stopped
           or Actual_State = Failed
         then
            Waiting_Duration := 60.0;
         else
            Next_State := Ref.Requested_State;
            if Ref.The_Account /= null then

               case Next_State is
               when Downloading =>
                  if Actual_State = Downloading then
                     Waiting_Duration := 0.0;
                     Old_Byte_Count   :=  New_Byte_Count;
                     Ref.The_Receiver.Get_Byte_Count(Current_Byte_Count);
                     New_Byte_Count := Saved_Byte_Count + Current_Byte_Count;
                     if New_Byte_Count > Old_Byte_Count then
                        Idle_Since := Now;
                        New_State  := Downloading;
                     else
                        Ref.The_Receiver.Get_Is_Running(Is_Running);
                        if not Is_Running then
                           Top_Protocols.Stop(Ref.The_Account.all);
                           Saved_Byte_Count := New_Byte_Count;
                           New_State        := Waiting;
                        end if;
                     end if;
                  elsif Actual_State = Sleeping then
                     New_State      := Waiting;
                     New_Byte_Count := Saved_Byte_Count + Current_Byte_Count;
                  else
                     if Actual_State /= Waiting
                       or else  (Now - Last_Connection_Attempt) > Waiting_Duration then
                        begin
                           Waiting_Duration := Waiting_Duration + 1.0;
                           Echo(Signature &  " : connecting");
                           Last_Connection_Attempt := Now;
                           Top_Protocols.Connect(Ref.The_Account.all);
                           File_Name := To_Unbounded_String(Top_Protocols.Get_File_Name(Ref.The_Account.all));
                           Output_Full_Name := To_Unbounded_String(Next_Output_Full_Name(To_String(Tmp_Path), To_String(File_Name)));
                           Top_Protocols.Start(Ref.The_Account.all);
                           declare
                              Bytes : Ada.Streams.Stream_Element_Array(1..Top_Protocols.Get_Header_Len(Ref.The_Account.all));
                              Total : Ada.Streams.Stream_Element_Count;
                              File  : Ada.Streams.Stream_IO.File_Type;
                           begin
                              Top_Protocols.Read(Ref.The_Account.all, Bytes, Total);
                              Ada.Streams.Stream_IO.Create(File, Ada.Streams.Stream_IO.Out_File, To_String(Output_Full_Name));
                              Ada.Streams.Stream_IO.Write(File, Bytes);
                              Ada.Streams.Stream_IO.Close(File);
                              Saved_Byte_Count := Saved_Byte_Count + Ada.Streams.Stream_IO.Count(Total);
                           end;

                           Ref.The_Receiver.Start( Ref.The_Account, To_String(Output_Full_Name));
                           Ref.The_Receiver.Get_Byte_Count(Current_Byte_Count);
                           New_State      := Downloading;
                           New_Byte_Count := Saved_Byte_Count + Current_Byte_Count;

                        exception
                           when Protocols.SERVER_ERROR =>
                              Echo(Signature & " : SERVER_ERROR while starting to download");
                              Ref.The_Receiver.Stop;
                              Ref.The_Receiver.Get_Byte_Count(Current_Byte_Count);
                              New_State      := Current_Status.The_State;
                              New_Byte_Count := Saved_Byte_Count + Current_Byte_Count;

                              Top_Protocols.Stop(Ref.The_Account.all);
                        end;
                     end if;
                  end if;

                  Current_Status := Status'(The_State => New_State,
                                            Byte_Count => New_Byte_Count);
                  Ref.The_Status.Set(Current_Status);
               when Stopped =>
                  if Actual_State = Stopped then
                     null;
                  else
                     Echo(Signature & " : Changing to Stopped");
                     Ref.The_Receiver.Stop;
                     Top_Protocols.Stop(Ref.The_Account.all);
                     Ref.The_Receiver.Get_Byte_Count(Current_Byte_Count);
                     Saved_Byte_Count := Saved_Byte_Count + Current_Byte_Count;
                     Current_Status := Status'(The_State => Stopped,
                                               Byte_Count => Saved_Byte_Count);
                     Ref.The_Status.Set(Current_Status);
                     Finish_Up(Ref);
                  end if;

               when Paused =>
                  case Actual_State is
                     when Paused =>
                        null;
                     when Stopped =>
                        null;
                     when others =>
                        Waiting_Duration := 0.0;
                        Ref.The_Receiver.Stop;
                        Top_Protocols.Stop(Ref.The_Account.all);
                        Ref.The_Receiver.Get_Byte_Count(Current_Byte_Count);
                        Saved_Byte_Count := Saved_Byte_Count + Current_Byte_Count;
                        Current_Status   := Status'(The_State => Paused,
                                                    Byte_Count => Saved_Byte_Count);
                        Ref.The_Status.Set(Current_Status);
                  end case;

                  when others =>
                     null;
               end case;

            else -- Ref.The_Account = null
               case Next_State is
                  when Downloading =>
                     Top_Protocols.Factory(Ref.The_Account, To_String(Ref.Source), Ref.Is_Live);
                     if  Ref.The_Account = null then
                        Ref.The_Status.Set_State(Failed);
                        Finish_Up(Ref);
                     else
                        Top_Protocols.Init(Ref.The_Account.all, To_String(Ref.Source));
                     end if;
                  when Stopped =>
                     if Ref.Is_Live then
                        Ref.The_Status.Set_State(Stopped);
                        Finish_Up(Ref);
                     end if;
                  when Paused =>
                     if Actual_State /= Stopped and Actual_State /= Paused then
                        Ref.The_Status.Set_State(Paused);
                     end if;
                  when others =>
                     null;
               end case;
            end if;
         end if;

         select
            accept Wake_Up do
               null;
            end Wake_Up;
         or
            accept Quit do
               Echo(Signature & " : Quit");
               Ref.The_Receiver.Quit;
               Remove_Account(Ref);
               Is_Enough := True;
            end Quit;
         or
            delay Waiting_Duration;
         end select;
      end loop;
   exception
      when E : others =>
         Echo (Signature & " : " & Ada.Exceptions.Exception_Information (E));
   end Controller;

   ------------
   -- Create --
   ------------

   procedure Create(Ref                : out Downloader_Access;
                    Source             : in  String;
                    Local_Path         : in  Path;
                    Estimated_Duration : in  Download_Duration;
                    Is_Live            : in  Boolean) is
   begin
      -- TODO: find out how to do the initialization of Ref with values.
      Ref                    := new Downloader;
      Ref.Source             := To_Unbounded_String(Source);
      Ref.Local_Path         := Local_Path;
      Ref.Estimated_Duration := Estimated_Duration;
      Ref.Is_Live            := Is_Live;

      Ref.The_Controller.Wake_Up;
   end Create;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status(This : in not null Downloader_Access) return Status is
      The_Status : Status := This.The_Status.Get;
   begin
      return The_Status;
   end Get_Status;

   ----------
   -- Stop --
   ----------

   procedure Stop(This : in not null Downloader_Access) is

   begin
      This.Requested_State := Stopped;
      select
         This.The_Controller.Wake_Up;
      or
         delay 0.01;
      end select;
   end Stop;

   -----------
   -- Start --
   -----------

   procedure Start(This : in not null Downloader_Access) is
   begin
      This.Requested_State := Downloading;
      select
         This.The_Controller.Wake_Up;
      or
         delay 0.01;
      end select;
   end Start;

   -----------
   -- Pause --
   -----------

   procedure Pause(This : in not null Downloader_Access) is
   begin
      This.Requested_State := Paused;
      select
         This.The_Controller.Wake_Up;
      or
         delay 0.01;
      end select;
   end Pause;

   ----------
   -- Quit --
   ----------

   procedure Quit(This : in not null Downloader_Access) is
   begin
      This.The_Controller.Quit;
   end Quit;

end Downloaders;

