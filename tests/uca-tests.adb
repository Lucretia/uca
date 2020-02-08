------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ada.Characters.Latin_1;
with Ada.Strings.UTF_Encoding;         use Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Strings; use Ada.Strings.UTF_Encoding.Strings;
with Ada.Unchecked_Conversion;

package body UCA.Tests is
   package L1 renames Ada.Characters.Latin_1;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "UCA.Tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Octets'Access, "Octets");
   end Initialize;

   function To_Octet is new Ada.Unchecked_Conversion (Source => Character, Target => Octets);

   procedure Test_Octets is
      A         : constant Octets          := Character'Pos ('A');    --  <= 7F
      STS       : constant Octets          := Character'Pos (L1.STS); --  >= 7F
      Copyright : constant String (1 .. 1) := (others => L1.Copyright_Sign);
      Copy_UTF  : constant UTF_String      := Encode (Copyright, UTF_8); --  Force otherwise compiler makes the length 4
   begin
      Ahven.Assert (Condition => (A and Octet_Mask_One) = A,
                    Message   => "A is not an octet and should be");

      Ahven.Assert (Condition => (STS and Octet_Mask_One) /= STS,
                    Message   => "STS is an octet and should not be");

      Ahven.Assert (Condition => Copy_UTF'Length = 2,
                    Message   => "Copy_UTF'Length /= 2");

      --  Copyright symbol, L1 value of 169
      --  C2 = 194 = 11 000010 = xx000010
      --  A9 = 169 = 10 101001 = xx101001
      --  Result   = 1010 1001 = 169
      Ahven.Assert (Condition => Character'Pos (Copy_UTF (Copy_UTF'First)) = 16#C2# and
                                 Character'Pos (Copy_UTF (Copy_UTF'Last))  = 16#A9#,
                    Message   => "Copy_UTF /= (C2, A9) and it should be");

      declare
         Copyright : constant Unicode_String (1 .. 2) := (1 => To_Octet (Copy_UTF (1)), 2 => To_Octet (Copy_UTF (2)));
      begin
         null;
      end;
   end Test_Octets;
end UCA.Tests;
