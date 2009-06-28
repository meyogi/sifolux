------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;
with System.Address_To_Access_Conversions;
with Interfaces.C;
with Interfaces.C.Pointers;
with interfaces.c.strings;
with Ada.Streams;
with Ada.Unchecked_Conversion;

with Utils;                 use Utils;

package body Protocols.MMS is

   package Stream_Element_Pointers is
     new Interfaces.C.Pointers
       (Ada.Streams.Stream_Element_Offset,
        Ada.Streams.Stream_Element,
        Ada.Streams.Stream_Element_Array,
        0);


   package C renames Interfaces.C;
   package Char_Ptrs is
     new C.Pointers (Index              => C.size_t,
                     Element            => C.char,
                     Element_Array      => C.char_array,
                     Default_Terminator => C.nul);

   -------------
   -- Connect --
   -------------

   procedure Connect(The_Account : in out Account; Source : in String) is
      use type System.Address;
      function Internal (io        :    System.Address; 
                         data      :    System.Address;
                         Source    : in String;
                         Bandwidth : in interfaces.c.int) return System.Address;
      pragma Import (C, Internal, "mms_connect");
   begin
      The_Account.P_Object := Internal(System.Null_Address, System.Null_Address,
                                       Source & ASCII.NUL,
                                       Interfaces.C.int(The_Account.Bandwidth));
      if The_Account.P_Object = System.Null_Address then
         raise Server_Error;
      end if;
   end Connect;

   -----------
   -- Close --
   -----------

   procedure Close(This : in out Account) is
      use type System.Address;
      procedure Internal (P_Object : in System.Address);
      pragma Import (C, Internal, "mms_close");
   begin
      Echo("Protocols.MMS.Close");
      Internal(This.P_Object);
   end Close;

   ----------
   -- Read --
   ----------

   procedure Read(Total :    out Ada.Streams.Stream_Element_Count;
                  This  : in out Account;
                  Data  : in out Ada.Streams.Stream_Element_Array;
                  Len   : in     Ada.Streams.Stream_Element_Count) is
      procedure Internal (Result   :    out Interfaces.C.int;
                          io       :        System.Address;
                          P_Object :        System.Address;
                          Data     : in out Ada.Streams.Stream_Element_Array;
                          Len      : in     Interfaces.C.int);
      pragma Import (C, Internal, "mms_read");
      pragma import_valued_procedure( Internal );

      use type Interfaces.C.int;

      Result : Interfaces.C.int;
   begin
      Internal(Result,
               System.Null_Address,
               This.P_Object,
               Data,
               Interfaces.C.int(Len));
      Total := Ada.Streams.Stream_Element_Count(Result);
   end Read;

   ------------------------
   -- Get_Asf_Header_Len --
   ------------------------

   function Get_Asf_Header_Len(This : in Account) return Ada.Streams.Stream_Element_Count is
      function Internal (P_Object : System.Address) return Interfaces.Unsigned_32;
      pragma Import (C, Internal, "mms_get_asf_header_len");
   begin
      return
        Ada.Streams.Stream_Element_Count(Internal(This.P_Object));
   end Get_Asf_Header_Len;

   ------------------------
   -- Get_Asf_Packet_Len --
   ------------------------

   function Get_Asf_Packet_Len(This : in Account) return Ada.Streams.Stream_Element_Count is
      function Internal (P_Object : System.Address) return Interfaces.Unsigned_64;
      pragma Import (C, Internal, "mms_get_asf_packet_len");
   begin
      return
        Ada.Streams.Stream_Element_Count(Internal(This.P_Object));
   end Get_Asf_Packet_Len;

end Protocols.MMS;
