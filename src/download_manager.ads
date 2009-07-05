------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;
with  Ada.Real_Time;
with Glib;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Task_Identification;
with Text_IO;
with Ada.Streams.Stream_IO;

with Utils;                 use Utils;
with Downloaders;

package Download_Manager is

   --------------
   -- Download --
   --------------

   type Download is private;

   subtype Download_Id  is Positive;

   type Recurrence        is (Once, Every_Day, Every_Week);
   type State             is (Sleeping, Downloading, Missed, Waiting, Finished, Paused,
                              Stopped, Failed);
   type Download_Id_Array is array (Positive range <>) of Download_Id;

   ----------------------
   -- Download_Summary --
   ----------------------

   type Download_Summary is
      record
         Id         : Download_Id;
         Name       : Ada.Strings.Unbounded.Unbounded_String;
         Byte_Count : Ada.Streams.Stream_IO.Count;
         Start_Time : Ada.Calendar.Time;
         Frequency  : Recurrence;
         The_State  : State;
      end record;


   package Download_Summary_Maps is
     new Ada.Containers.Ordered_Maps
       (Element_Type => Download_Summary ,
        Key_Type     => Download_Id );


   Procedure Init;
   function Get_Last_Changes return Download_Summary_Maps.Map;

   procedure Modify(Id : in Download_Id; D : in Download);
   procedure Add(D : in Download);
   procedure Delete(Id : in Download_Id);
   -- Deletes the download which is designated by Id from the list of downloads.
   -- The downloaded files are not deleted from the disk.
   -- If Id is does not designate a download, nothing happens.

   procedure Pause(Id : in Download_Id);
   procedure Stop(Id : in Download_Id);
   procedure Start(Id : in Download_Id);
   procedure Run;
   procedure Quit;

   function Get(Id : in Download_Id) return Download;
   function Exists(Id : in Download_Id) return Boolean;
   -- Returns True if Id designates an element, and returns False otherwise.

   procedure Init(New_Download : out Download);
   procedure Init_Like(New_Download : out Download; Old_Download : in Download);
   function Get_Name(D : in Download) return String ;
   procedure Set_Name(D : in out Download; Name : in String);
   function Get_Id(D : in Download) return Download_Id;
   function Get_Requested_Start_Time(D : in Download) return Ada.Calendar.Time;
   procedure Set_Requested_Start_Time(D : in out Download; Start_Time: in Ada.Calendar.Time);
   function Get_Frequency(D : in Download) return Recurrence;
   procedure Set_Frequency(D : in out Download; Frequency : in Recurrence);
   function Get_Minutes(D: in Download) return Downloaders.Download_Duration;
   procedure Set_Minutes(D: in out Download; Minutes : in Downloaders.Download_Duration);
   function Get_Address(D : in Download) return String;
   procedure Set_Address(D : in out Download; Source : in String);
   function Is_Live(D : in Download) return Boolean;
   procedure Set_Live(D : in out Download; Is_Live : in Boolean);
   function Get_State(D : in Download) return State;
   function Is_New(D : in Download) return Boolean;
   function Is_Duration_Modifiable(D : in Download) return Boolean;
   function Is_Deletable(D : in Download) return Boolean;
   -- Returns True if D can be deleted, and returns False otherwise.

   function Is_Url_Valid(Address : in String) return Boolean;
   function Is_Protocol_Supported(Address : in String) return Boolean;

private
   type Text_File_Access is access Text_IO.File_Type;

   type Download is
      record
         Id                   : Download_Id;
         Is_New               : Boolean;
         Name                 : Unbounded_String;
         Byte_Count           : Ada.Streams.Stream_IO.Count;
         Requested_Start_Time : Ada.Calendar.Time;
         Frequency            : Recurrence;
         Minutes              : Downloaders.Download_Duration;
         Source               : Unbounded_String;
         Is_Live              : Boolean;
         The_State            : State;
         Action_Time          : Ada.Calendar.Time;
         Actual_Start_Time    : Ada.Calendar.Time;
         P_Downloader         : Downloaders.Downloader_Access;
         Local_Path           : Downloaders.Path;
         Is_Changed           : Boolean := True;
      end record;

end Download_Manager;
