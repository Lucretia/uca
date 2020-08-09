------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;
-- with Ada.Strings.UTF_Encoding;

package UCA.ANSI is
   Escape              : constant Octets;
   Left_Square_Bracket : constant Octets;
   Semicolon           : constant Octets;

   CSI : constant Unicode_String;

   --  3/4-bit colours.
   type Basic_Colours is (Black,
                          Red,
                          Green,
                          Yellow,
                          Blue,
                          Magenta,
                          Cyan,
                          White,
                          Gray,           --  "Bright black"
                          Bright_Red,
                          Bright_Green,
                          Bright_Yellow,
                          Bright_Blue,
                          Bright_Magenta,
                          Bright_Cyan,
                          Bright_White);

   function Reset return Unicode_String is (CSI & "0m");

   function SGR (Foreground : in Basic_Colours;
                 Background : in Basic_Colours;
                 Defaults   : in Boolean := False) return Unicode_String;

   function Set_Foreground (Colour : in Basic_Colours) return Unicode_String with
     Inline;

   function Set_Background (Colour : in Basic_Colours) return Unicode_String with
     Inline;


   --  8-bit colours, the first 16 values are the above colours.
   type Colours is range 0 .. 255;

   function SGR (Foreground : in Colours;
                 Background : in Colours;
                 Defaults   : in Boolean := False) return Unicode_String;

   function Set_Foreground (Colour : in Colours) return Unicode_String with
     Inline;

   function Set_Background (Colour : in Colours) return Unicode_String with
     Inline;


   --  For true colour support, RGB.
   type Colour_Components is range 0 .. 255;

   type RGB_Colours is record
      Red   : Colour_Components;
      Green : Colour_Components;
      Blue  : Colour_Components;
   end record;

   function SGR (Foreground : in RGB_Colours;
                 Background : in RGB_Colours;
                 Defaults   : in Boolean := False) return Unicode_String;

   function Set_Foreground (Colour : in RGB_Colours) return Unicode_String with
     Inline;

   function Set_Background (Colour : in RGB_Colours) return Unicode_String with
     Inline;

   function Set_Foreground (Red   : in Colour_Components;
                            Green : in Colour_Components;
                            Blue  : in Colour_Components) return Unicode_String is
     (Set_Foreground (RGB_Colours'(Red, Green, Blue)));

   function Set_Background (Red   : in Colour_Components;
                            Green : in Colour_Components;
                            Blue  : in Colour_Components) return Unicode_String is
     (Set_Background (RGB_Colours'(Red, Green, Blue)));

   --  Other functions.
   function Clear return Unicode_String is (+"2J");

   function Set_Location (X : in Natural; Y : in Natural) return Unicode_String with
     Inline;
private
   package L1 renames Ada.Characters.Latin_1;

   subtype C0_Set is Octets range 16#00# .. 16#1F#;
   subtype C1_Set is Octets range 16#80# .. 16#9F#;

   function Convert is new Ada.Unchecked_Conversion (Source => Character, Target => Octets);

   Escape              : constant Octets := Convert (L1.ESC);
   Left_Square_Bracket : constant Octets := Convert (L1.Left_Square_Bracket);
   Semicolon           : constant Octets := Convert (L1.Semicolon);

   CSI : constant Unicode_String := Escape & Left_Square_Bracket;
end UCA.ANSI;
