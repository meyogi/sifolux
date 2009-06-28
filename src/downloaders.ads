------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;

with Utils; use Utils;
with Top_Protocols;

package Downloaders is

   ----------------
   -- Downloader --
   ----------------

   type Downloader        is limited private;

   type Downloader_Access is access Downloader;
   type State             is (Waiting, Downloading, Paused, Stopped, Sleeping,
                              Failed, Finished);
   subtype Download_Duration is Positive range 1..60_000;
   subtype Path              is Ada.Strings.Unbounded.Unbounded_String;

   ------------
   -- Status --
   ------------
   type Status is
      record
         The_State  : State;
         Byte_Count : Ada.Streams.Stream_IO.Count;
      end record;

   procedure Create(Ref                : out Downloader_Access;
                    Source             : in  String;
                    Local_Path         : in  Path;
                    Estimated_Duration : in  Download_Duration;
                    Is_Live            : in  Boolean);

   function Get_Status(This : in not null Downloader_Access) return Status;
   procedure Stop(This : in not null Downloader_Access);
   procedure Start(This : in not null Downloader_Access);
   procedure Pause(This : in not null Downloader_Access);
   procedure Quit(This : in not null Downloader_Access);

private

   task type Receiver is
      entry Start(Account   : in Top_Protocols.Account_Class_Access;
                  Full_Name : in String);
      entry Stop;
      entry Get_Byte_Count(Count : out Ada.Streams.Stream_IO.Count);
      entry Get_Is_Running(Running : out Boolean);
      entry Quit;
   end Receiver;

   task type Controller(Ref : access Downloader) is
      entry Wake_Up;
      entry Quit;
   end Controller;

   protected type Protected_Status is
      function Get return Status;
      procedure Set(New_Status : in Status);
      procedure Set_State(New_State : in State);
   private
      Current_Status : Status := Status'(Sleeping, 0);
   end Protected_Status;

   type Downloader is
      record
         Source             : Unbounded_String;
         Local_Path         : Path;
         Estimated_Duration : Download_Duration;
         Is_Live            : Boolean;
         The_Status         : Protected_Status;
         The_Account        : Top_Protocols.Account_Class_Access;
         The_Controller     : Controller(Downloader'Access);
         The_Receiver       : Receiver;
         Requested_State    : State := Sleeping;

      end record;


end Downloaders;
