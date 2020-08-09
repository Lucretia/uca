------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Ada.Iterator_Interfaces;
with Ada.Strings.UTF_Encoding;

package UCA is
   -- pragma Preelaborate;

   Unicode_Parse_Error : exception;

   type Octets is mod 2 ** 8 with
     Size => 8;

   type Unicode_String is array (Positive range <>) of Octets with
     Pack => True;

   type Unicode_String_Access is access all Unicode_String;
   type Unicode_String_Constant_Access is access constant Unicode_String;

   type All_Code_Points is mod 2 ** 32 with
     Size => 32;

   --  Surrogate values for UTF-16.
   subtype High_Surrogates is All_Code_Points range 16#D800# .. 16#DBFF#;
   subtype Low_Surrogates  is All_Code_Points range 16#DC00# .. 16#DFFF#;

   --  This should match Wide_Wide_Character in size.
   --    See 3.9 of the standard for the range.
   --      The range is 0 .. 10_FFFF
   type Code_Points is new All_Code_Points range 0 .. 16#0010_FFFF# with
     Static_Predicate => All_Code_Points (Code_Points) not in High_Surrogates and
                         All_Code_Points (Code_Points) not in Low_Surrogates,
     Size             => 32;

   type Unicode_Cursor is limited private;

   function Has_Element (Position : in Unicode_Cursor) return Boolean;

   package Unicode_Iterators is new Ada.Iterator_Interfaces (Unicode_Cursor, Has_Element);

   --  Provide a way to do user-defined iteration.
   type Unicode_Reference (Container : access constant Unicode_String) is tagged null record with
     Implicit_Dereference => Container,
     Default_Iterator     => Iterate,
     Iterator_Element     => Code_Points,
     Constant_Indexing    => Element_Value;

   function Iterate (Container : in Unicode_Reference) return Unicode_Iterators.Forward_Iterator'Class;

   function Element_Value (Container : in Unicode_Reference; Position : in Unicode_Cursor) return Code_Points;

   function Element (Position : in Unicode_Cursor) return Code_Points;

   --  TODO: Should this use Encode?
   function "&" (Left : in Ada.Strings.UTF_Encoding.UTF_String; Right : in Unicode_String) return Unicode_String;
   function "&" (Left : in Unicode_String; Right : in Ada.Strings.UTF_Encoding.UTF_String) return Unicode_String;

   function "&" (Left : in Ada.Strings.UTF_Encoding.UTF_String; Right : in Octets) return Unicode_String with
     Inline;

   function "+" (Item : in String) return Unicode_String with
     Inline;
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

   Octet_Header_One   : constant Octets := 2#0000_0000#;
   Octet_Header_Two   : constant Octets := 2#1100_0000#;
   Octet_Header_Three : constant Octets := 2#1110_0000#;
   Octet_Header_Four  : constant Octets := 2#1111_0000#;
   Octet_Follower     : constant Octets := 2#1000_0000#;

   --  Mask to get the data from an octet.
   Octet_Data_Mask_One          : constant Octets := 2#0111_1111#;
   Octet_Data_Mask_Two          : constant Octets := 2#0001_1111#;
   Octet_Data_Mask_Three        : constant Octets := 2#0000_1111#;
   Octet_Data_Mask_Four         : constant Octets := 2#0000_0111#;
   Octet_Data_Mask_Follower     : constant Octets := 2#0011_1111#;

   type Unicode_Cursor is record
      Container  : access constant Unicode_String;
      Index      : Natural;
      Next_Index : Natural;
      Code       : Code_Points;
   end record;

   No_Element : constant Unicode_Cursor := (Container => null, Code => 0, others => Natural'First);

   type Unicode_Iterator is new Unicode_Iterators.Forward_Iterator with record
      Container : access constant Unicode_String;
   end record;

   overriding
   function First (Object : in Unicode_Iterator) return Unicode_Cursor;

   overriding
   function Next (Object : in Unicode_Iterator; Position : in Unicode_Cursor) return Unicode_Cursor;

   function Convert is new Ada.Unchecked_Conversion (Source => Octets, Target => Character);
   function Convert is new Ada.Unchecked_Conversion (Source => Character, Target => Octets);
end UCA;
