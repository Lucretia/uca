------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
use Ada.Strings;

package body UCA.ANSI is
   function Get_Foreground_Code (Colour : in Basic_Colours) return Unicode_String is
     (case Colour is
         when Black          => +"30",
         when Red            => +"31",
         when Green          => +"32",
         when Yellow         => +"33",
         when Blue           => +"34",
         when Magenta        => +"35",
         when Cyan           => +"36",
         when White          => +"37",
         when Gray           => +"90",
         when Bright_Red     => +"91",
         when Bright_Green   => +"92",
         when Bright_Yellow  => +"93",
         when Bright_Blue    => +"94",
         when Bright_Magenta => +"95",
         when Bright_Cyan    => +"96",
         when Bright_White   => +"97");

   function Get_Background_Code (Colour : in Basic_Colours) return Unicode_String is
    (case Colour is
        when Black          => +"40",
        when Red            => +"41",
        when Green          => +"42",
        when Yellow         => +"43",
        when Blue           => +"44",
        when Magenta        => +"45",
        when Cyan           => +"46",
        when White          => +"47",
        when Gray           => +"100",
        when Bright_Red     => +"101",
        when Bright_Green   => +"102",
        when Bright_Yellow  => +"103",
        when Bright_Blue    => +"104",
        when Bright_Magenta => +"105",
        when Bright_Cyan    => +"106",
        when Bright_White   => +"107");

   function SGR (Foreground : in Basic_Colours;
                 Background : in Basic_Colours;
                 Defaults   : in Boolean := False) return Unicode_String is
   begin
      if Defaults then
         return Reset;
      end if;

      return Get_Foreground_Code (Foreground) & Semicolon & Get_Background_Code (Background) & "m";
   end SGR;

   function Set_Foreground (Colour : in Basic_Colours) return Unicode_String is
   begin
      return Get_Foreground_Code (Colour) & "m";
   end Set_Foreground;

   function Set_Background (Colour : in Basic_Colours) return Unicode_String is
   begin
      return Get_Background_Code (Colour) & "m";
   end Set_Background;


   Foreground_Code : constant String := "38";
   Background_Code : constant String := "48";

   function To_String (Colour : in Colours) return String is (Trim (Colours'Image (Colour), Both));

   function SGR (Foreground : in Colours;
                 Background : in Colours;
                 Defaults   : in Boolean := False) return Unicode_String is

   begin
      if Defaults then
         return Reset;
      end if;

      return
        Foreground_Code & "5" & Semicolon & To_String (Foreground) & Semicolon &
        Background_Code & "5" & Semicolon & To_String (Background) & "m";
   end SGR;

   function Set_Foreground (Colour : in Colours) return Unicode_String is
   begin
      return Foreground_Code & Semicolon & "5" & Semicolon & To_String (Colour) & "m";
   end Set_Foreground;

   function Set_Background (Colour : in Colours) return Unicode_String is
   begin
      return Background_Code & Semicolon & "5" & Semicolon & To_String (Colour) & "m";
   end Set_Background;


   function To_String (Colour : in Colour_Components) return String is (Trim (Colour_Components'Image (Colour), Both));

   function SGR (Foreground : in RGB_Colours;
                 Background : in RGB_Colours;
                 Defaults   : in Boolean := False) return Unicode_String is
   begin
      if Defaults then
         return Reset;
      end if;

      return
        Foreground_Code & "2" & Semicolon &
          To_String (Foreground.Red) & Semicolon &
          To_String (Foreground.Green) & Semicolon &
          To_String (Foreground.Blue) & Semicolon &
        Background_Code & "2" & Semicolon &
          To_String (Background.Red) & Semicolon &
          To_String (Background.Green) & Semicolon &
          To_String (Background.Blue) & Semicolon &
        "m";
   end SGR;

   function Set_Foreground (Colour : in RGB_Colours) return Unicode_String is
   begin
      return Foreground_Code & "2" & Semicolon &
        To_String (Colour.Red) & Semicolon &
        To_String (Colour.Green) & Semicolon &
        To_String (Colour.Blue) &
        "m";
   end Set_Foreground;

   function Set_Background (Colour : in RGB_Colours) return Unicode_String is
   begin
      return Background_Code & "2" & Semicolon &
        To_String (Colour.Red) & Semicolon &
        To_String (Colour.Green) & Semicolon &
        To_String (Colour.Blue) &
        "m";
   end Set_Background;


   function Set_Location (X : in Natural; Y : in Natural) return Unicode_String is
      function To_String (Value : in Natural) return String is (Trim (Natural'Image (Value), Both));
   begin
      return To_String (X) & Semicolon & To_String (Y) & "H";
   end Set_Location;
end UCA.ANSI;
