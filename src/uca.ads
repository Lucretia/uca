------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
-- with Ada.Unchecked_Conversion;

package UCA is
   pragma Preelaborate;

   type Octets is mod 2 ** 8 with
     Size => 8;

   type Unicode_String is array (Positive range <>) of Octets with
     Pack => True;

   type Unicode_String_Access is access all Unicode_String;

   type All_Code_Points is mod 2 ** 32 with
     Size => 32;

   --  Surrogate values for UTF-16.
   subtype High_Surrogates is All_Code_Points range 16#D800# .. 16#DBFF#;
   subtype Low_Surrogates  is All_Code_Points range 16#DC00# .. 16#DFFF#;

   --  This should match Wide_Wide_Character in size.
   --    See 3.9 of the standard for the range.
   --      The range is 0 .. 10_FFFF
   type Code_Points is new All_Code_Points with
     Static_Predicate => All_Code_Points (Code_Points) not in High_Surrogates and
                         All_Code_Points (Code_Points) not in Low_Surrogates,
     Size             => 32;
private
   --  No. of bytes | Bits for code point | First   | Last     |  Byte 1  |  Byte 2  |  Byte 3  |  Byte 4
   --        1      |           7         | U+0000  | U+007F   | 0wwwwwww |
   --        2      |          11         | U+0080  | U+07FF   | 110wwwww | 10xxxxxx
   --        3      |          16         | U+0800  | U+FFFF   | 1110wwww | 10xxxxxx | 10yyyyyy
   --        4      |          21         | U+10000 | U+10FFFF | 11110www | 10xxxxxx | 10yyyyyy | 10zzzzzz
   --
   --  The above map onto 32 bits:
   --
   --        1      |           7         | 0000 0000 0000 0000 0000 0000 0www wwww
   --        2      |          11         | 0000 0000 0000 0000 0000 0www wwxx xxxx
   --        3      |          16         | 0000 0000 0000 0000 wwww xxxx xxyy yyyy
   --        4      |          21         | 0000 0000 000w wwxx xxxx yyyy yyzz zzzz

   --  Mask to determing the type of octet.
   Octet_Header_Mask_One   : constant Octets := 2#1000_0000#;
   Octet_Header_Mask_Two   : constant Octets := 2#1110_0000#;
   Octet_Header_Mask_Three : constant Octets := 2#1111_0000#;
   Octet_Header_Mask_Four  : constant Octets := 2#1111_1000#;
   Octet_Follower_Mask     : constant Octets := 2#1100_0000#;

   --  Mask to get the data from an octet.
   Octet_Mask_One          : constant Octets := 2#0111_1111#;
   Octet_Mask_Two          : constant Octets := 2#0001_1111#;
   Octet_Mask_Three        : constant Octets := 2#0000_1111#;
   Octet_Mask_Four         : constant Octets := 2#0000_0111#;

--     --  Various representations of code points.
--     type Code_Point_Data is mod 2 ** 32 with
--       Static_Predicate => Code_Point in 0 .. 16#0010_FFFF#,
--       Size             => 32;
--
--     type Code_Point_1 is
--        record
--           Zero : Natural;  --  Should always be zero!
--           Data : Natural;  --  Octet_1
--        end record with
--       Convention => C,
--       Size       => Code_Point_Data'Size;
--
--     for Code_Point_1 use
--        record
--           Zero at 0 range 7 .. 31;
--           Data at 0 range 0 .. 6;
--        end record;
--
--     type Code_Point_2 is
--        record
--           Zero   : Natural;  --  Should always be zero!
--           Data_2 : Natural;  --  Octet_2
--           Data_1 : Natural;  --  Octet_Continuation
--        end record with
--       Convention => C,
--       Size       => Code_Point_Data'Size;
--
--     for Code_Point_2 use
--        record
--           Zero   at 0 range 11 .. 31;
--           Data_2 at 0 range  6 .. 10;
--           Data_1 at 0 range  0 ..  5;
--        end record;
--
--     type Code_Point_3 is
--        record
--           Zero   : Natural;  --  Should always be zero!
--           Data_3 : Natural;  --  Octet_3
--           Data_2 : Natural;  --  Octet_Continuation
--           Data_1 : Natural;  --  Octet_Continuation
--        end record with
--       Convention => C,
--       Size       => Code_Point_Data'Size;
--
--     for Code_Point_3 use
--        record
--           Zero   at 0 range 16 .. 31;
--           Data_3 at 0 range 12 .. 15;
--           Data_2 at 0 range  6 .. 11;
--           Data_1 at 0 range  0 ..  5;
--        end record;
--
--     type Code_Point_4 is
--        record
--           Zero   : Natural;  --  Should always be zero!
--           Data_4 : Natural;  --  Octet_4
--           Data_3 : Natural;  --  Octet_Continuation
--           Data_2 : Natural;  --  Octet_Continuation
--           Data_1 : Natural;  --  Octet_Continuation
--        end record with
--       Convention => C,
--       Size       => Code_Point_Data'Size;
--
--     for Code_Point_4 use
--        record
--           Zero   at 0 range 21 .. 31;
--           Data_4 at 0 range 18 .. 20;
--           Data_3 at 0 range 12 .. 17;
--           Data_2 at 0 range  6 .. 11;
--           Data_1 at 0 range  0 ..  5;
--        end record;
--
--     type Code_Points_Kinds is (One, Two, Three, Four, Code_Point);
--
--     type Code_Points (Kind : Code_Points_Kinds) is
--        record
--           case Kind is
--           when One =>
--              Code : Code_Point_1;
--
--           when Two =>
--              Code : Code_Point_2;
--
--           when Three =>
--              Code : Code_Point_3;
--
--           when Four =>
--              Code : Code_Point_4;
--
--           when Code_Points =>
--              Code : Code_Point_Data;
--           end case;
--        end record with
--       Convention => C,
--       Size       => Code_Point_Data'Size;
--
--     --     type ASCII_Range is new Octet range 0 .. 16#7F#;
--
--     function "&" (Left : in Unicode_String; Right : in Code_Point) return Unicode_String;
--     function "&" (Left : in Code_Point; Right : in Unicode_String) return Unicode_String;
--
--     function "&" (Left : in Unicode_String; Right : in Character) return Unicode_String;
--     function "&" (Left : in Character; Right : in Unicode_String) return Unicode_String;
   --  private
end UCA;
