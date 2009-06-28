------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Interfaces.C;
with Ada.Strings.Unbounded;
with System;
with Ada.Streams;

with Utils; use Utils;

package Protocols.MMS is

   -------------
   -- Account --
   -------------

   type Account is limited private;
   type Account_Access is access Account;

   procedure Connect(The_Account : in out Account; Source : in String);
   procedure Close(This : in out Account);
   procedure Read(Total :    out Ada.Streams.Stream_Element_Count;
                  This  : in out Account;
                  Data  : in out Ada.Streams.Stream_Element_Array;
                  Len   : in     Ada.Streams.Stream_Element_Count);
   function Get_Asf_Header_Len(This : in Account) return Ada.Streams.Stream_Element_Count;
   function Get_Asf_Packet_Len(This : in Account) return Ada.Streams.Stream_Element_Count;

private

   type Account is limited
      record
         Source    : Ada.Strings.Unbounded.Unbounded_String;
         Bandwidth : Positive := 1_000_000;
         P_Object  : System.Address := System.Null_Address;
      end record;

end Protocols.MMS;
