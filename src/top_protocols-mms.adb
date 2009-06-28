------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;
with Ada.Unchecked_Conversion;
with Interfaces;

with Protocols.MMS;
with Utils; use Utils;

package body Top_Protocols.MMS is

   ---------------
   -- To_String --
   ---------------

   function To_String
     (A : in Ada.Streams.Stream_Element_Array) return String is
      subtype Source is Ada.Streams.Stream_Element_Array(A'Range);
      subtype Result is String (Integer(A'First) .. Integer(A'Last));
      function To_St is new Ada.Unchecked_Conversion (Source, Result);
   begin
      return To_St (A);
   end To_String;

   --------------------
   -- Get_Header_Len --
   --------------------

   function Get_Header_Len(This : in MMS_Account) return Ada.Streams.Stream_Element_Count is
   begin
      return Protocols.MMS.Get_Asf_Header_Len(This.Acc);
   end Get_Header_Len;

   --------------------
   -- Get_Packet_Len --
   --------------------

   function Get_Packet_Len(This : in MMS_Account) return Ada.Streams.Stream_Element_Count is
   begin
      return Protocols.MMS.Get_Asf_Packet_Len(This.Acc);
   end Get_Packet_Len;

   ----------
   -- Read --
   ----------

   procedure Read(This  :  in out MMS_Account;
                  Bytes :    out Ada.Streams.Stream_Element_Array;
                  Total :    out Ada.Streams.Stream_Element_Count) is
   begin
      Protocols.MMS.Read(Total, This.Acc, Bytes, Bytes'Length);
   end Read;

   -------------
   -- Connect --
   -------------

   procedure Connect(This : in out MMS_Account) is
   begin
      Protocols.MMS.Connect(This.Acc, To_String(This.Source));
      This.Is_Running := True;
   end Connect;

   -----------
   -- Start --
   -----------

   procedure Start(This : in out MMS_Account) is
   begin
      null;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop(This : in out MMS_Account) is
   begin
      if This.Is_Running then
         Protocols.MMS.Close(This.Acc);
         This.Is_Running := False;
      end if;
   end Stop;

   ----------
   -- Init --
   ----------

   procedure Init(This   :  out MMS_Account;
                  Source :  in  String) is
   begin
      This.Source := Ada.Strings.Unbounded.To_Unbounded_String(Source);
   end Init;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name(This : in MMS_Account) return String is
   begin
      return "filename.asf";
   end Get_File_Name;

end Top_Protocols.MMS;
