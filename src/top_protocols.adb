------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;

with Top_Protocols.MMS;

package body Top_Protocols is

   -------------
   -- Factory --
   -------------

   procedure Factory(New_Account : out Account_Class_Access;
                     Source      : in  String;
                     Is_Live     : in  Boolean) is
   begin
      New_Account := new Top_Protocols.MMS.MMS_Account;
   end Factory;

   ----------
   -- Init --
   ----------

   procedure Init(This : out Account; Source : in String) is
   begin
      This.Source    := To_Unbounded_String(Source);
      This.File_Name := Null_Unbounded_String;
   end Init;

   ----------
   -- Quit --
   ----------

   procedure Quit(This : in out Account) is
   begin
      null;
   end Quit;

   -----------
   -- Start --
   -----------

   procedure Start(This : in out Account) is
   begin
      null;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop(This : in out Account) is
   begin
      null;
   end Stop;

   -------------
   -- Connect --
   -------------

   procedure Connect(This : in out Account) is
   begin
      null;
   end Connect;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name(This : in Account) return String is
   begin
      return To_String(This.File_Name);
   end Get_File_Name;

   --------------------
   -- Get_Header_Len --
   --------------------

   function Get_Header_Len(This : in Account) return Ada.Streams.Stream_Element_Count is
   begin
      return 0;
   end Get_Header_Len;

   --------------------
   -- Get_Packet_Len --
   --------------------

   function Get_Packet_Len(This : in Account) return Ada.Streams.Stream_Element_Count is
   begin
      return 0;
   end Get_Packet_Len;

   ----------
   -- Read --
   ----------

   procedure Read(This  :  in out Account;
                  Bytes :     out Ada.Streams.Stream_Element_Array;
                  Total :     out Ada.Streams.Stream_Element_Count) is
   begin
      null;
   end Read;

end Top_Protocols;
