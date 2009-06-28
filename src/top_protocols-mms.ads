------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Protocols.MMS;

package Top_Protocols.MMS is

   -----------------
   -- MMS_Account --
   -----------------

   type MMS_Account is new Account with private;

   overriding
   procedure Connect(This : in out MMS_Account);

   overriding
   procedure Start(This : in out MMS_Account);

   overriding
   procedure Stop(This : in out MMS_Account);

   overriding
   procedure Init(This   : out MMS_Account;
                  Source : in  String);

   overriding
   function Get_File_Name(This : in MMS_Account) return String;

   overriding
   function Get_Header_Len(This : in MMS_Account) return Ada.Streams.Stream_Element_Count;

   overriding
   function Get_Packet_Len(This : in MMS_Account) return Ada.Streams.Stream_Element_Count;

   overriding
   procedure Read(This  : in out MMS_Account;
                  Bytes :    out Ada.Streams.Stream_Element_Array;
                  Total :    out Ada.Streams.Stream_Element_Count);

private

   type MMS_Account is new Account with
      record
         Acc : Protocols.MMS.Account;
      end record;

end Top_Protocols.MMS;
