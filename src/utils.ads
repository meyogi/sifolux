------------------------------------------------------------------------------
--                                 Sifolux                                  --
--                                                                          --
--                         Copyright (C) 2009, YoGi                         --
-- See the file COPYING for copying permission.                             --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting;
with Text_IO;
with Interfaces.C.Strings;

package Utils is

   function Day_Of_Week(Date       : in Ada.Calendar.Time;
                        Time_Zone  : in Ada.Calendar.Time_Zones.Time_Offset := 0)
                        return Ada.Calendar.Formatting.Day_Name;

   function Local_Time_Of (Year   : in Ada.Calendar.Year_Number;
                           Month  : in Ada.Calendar.Month_Number;
                           Day    : in Ada.Calendar.Day_Number;
                           Hour   : in Ada.Calendar.Formatting.Hour_Number;
                           Minute : in Ada.Calendar.Formatting.Minute_Number;
                           Second : in Ada.Calendar.Formatting.Second_Number)
                           return Ada.Calendar.Time;

   procedure Echo(Item : in String);

end Utils;
