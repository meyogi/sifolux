------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;

package body Utils is
   use Ada.Calendar;

   -----------------
   -- Day_Of_Week --
   -----------------

   function Day_Of_Week(Date      : in Ada.Calendar.Time;
                        Time_Zone : in Ada.Calendar.Time_Zones.Time_Offset := 0)
                        return Ada.Calendar.Formatting.Day_Name is
      Y        : Year_Number       := Formatting.Year(Date, Time_Zone);
      M        : Month_Number      := Formatting.Month(Date, Time_Zone);
      D        : Day_Number        := Formatting.Day(Date, Time_Zone);
      The_Time : Ada.Calendar.Time := Formatting.Time_Of(Y, M, D);
   begin
      return Formatting.Day_Of_Week(The_Time);
   end Day_Of_Week;

   -------------------
   -- Local_Time_Of --
   -------------------

   function Local_Time_Of (Year   : in Ada.Calendar.Year_Number;
                           Month  : in Ada.Calendar.Month_Number;
                           Day    : in Ada.Calendar.Day_Number;
                           Hour   : in Ada.Calendar.Formatting.Hour_Number;
                           Minute : in Ada.Calendar.Formatting.Minute_Number;
                           Second : in Ada.Calendar.Formatting.Second_Number)
                           return Ada.Calendar.Time is
      use  Ada.Calendar;

      Time1  : Ada.Calendar.Time;
      Time2  : Ada.Calendar.Time;
      Offset : Time_Zones.Time_Offset;
   begin
      Time1  := Formatting.Time_Of(
                                   Year,
                                   Month,
                                   Day,
                                   Hour,
                                   Minute,
                                   Second,
                                   Time_Zone => Time_Zones.UTC_Time_Offset );
      Offset := Time_Zones.UTC_Time_Offset(Time1);
      Time2  := Formatting.Time_Of(
                                   Year,
                                   Month,
                                   Day,
                                   Hour,
                                   Minute,
                                   Second,
                                   Time_Zone => Offset );

      return Time2;
   end Local_Time_Of;

   ----------
   -- Echo --
   ----------

   procedure Echo(Item : in  String) renames Text_IO.Put_Line;

end Utils;
