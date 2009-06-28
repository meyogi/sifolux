------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Streams;

package Top_Protocols is

   -------------
   -- Account --
   -------------

   type Account is tagged limited private;
   type Account_Class_Access is access Account'Class;

   procedure Factory(New_Account : out Account_Class_Access;
                     Source      : in  String;
                     Is_Live     : in  Boolean);


   procedure Init(This : out Account; Source : in String);
   procedure Start(This : in out Account);
   procedure Stop(This : in out Account);
   function Get_File_Name(This : in Account) return String;
   procedure Connect(This : in out Account);
   function Get_Header_Len(This : in Account) return Ada.Streams.Stream_Element_Count;
   function Get_Packet_Len(This : in Account) return Ada.Streams.Stream_Element_Count;
   procedure Read(This  : in out Account;
                  Bytes :    out Ada.Streams.Stream_Element_Array;
                  Total :    out Ada.Streams.Stream_Element_Count);
private

   type Account is tagged limited
      record
         Source     : Ada.Strings.Unbounded.Unbounded_String;
         File_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Is_Running : Boolean := False;
      end record;

end Top_Protocols;
