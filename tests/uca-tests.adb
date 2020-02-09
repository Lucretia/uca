------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package body UCA.Tests is
   package L1 renames Ada.Characters.Latin_1;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "UCA.Tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Octets'Access, "Octets");
      Ahven.Framework.Add_Test_Routine (T, Test_Iterator'Access, "Iterator");
   end Initialize;

   procedure Test_Octets is
      A   : constant Octets := Character'Pos ('A');    --  <= 7F
      STS : constant Octets := Character'Pos (L1.STS); --  >= 7F
   begin
      Ahven.Assert (Condition => (A and Octet_Data_Mask_One) = A,
                    Message   => "A is not an octet and should be");

      Ahven.Assert (Condition => (STS and Octet_Data_Mask_One) /= STS,
                    Message   => "STS is an octet and should not be");
   end Test_Octets;

   procedure Test_Iterator is
      Runes : aliased UCA.Unicode_String :=
        (225, 154, 160, 225, 155, 135, 225, 154, 187, 225, 155, 171, 225, 155, 146, 225, 155, 166, 225,
         154, 166, 225, 155, 171, 225, 154, 160, 225, 154, 177, 225, 154, 169, 225, 154, 160, 225, 154,
         162, 225, 154, 177, 225, 155, 171, 225, 154, 160, 225, 155, 129, 225, 154, 177, 225, 154, 170,
         225, 155, 171, 225, 154, 183, 225, 155, 150, 225, 154, 187, 225, 154, 185, 225, 155, 166, 225,
         155, 154, 225, 154, 179, 225, 154, 162, 225, 155, 151);
      Hindi : aliased UCA.Unicode_String :=
        (224, 164, 174, 224, 165, 136, 224, 164, 130, 32, 224, 164, 149, 224, 164, 190, 224, 164, 129,
         224, 164, 154, 32, 224, 164, 150, 224, 164, 190, 32, 224, 164, 184, 224, 164, 149, 224, 164,
         164, 224, 164, 190, 32, 224, 164, 185, 224, 165, 130, 224, 164, 129, 32, 224, 164, 148, 224,
         164, 176, 32, 224, 164, 174, 224, 165, 129, 224, 164, 157, 224, 165, 135, 32, 224, 164, 137,
         224, 164, 184, 224, 164, 184, 224, 165, 135, 32, 224, 164, 149, 224, 165, 139, 224, 164, 136,
         32, 224, 164, 154, 224, 165, 139, 224, 164, 159, 32, 224, 164, 168, 224, 164, 185, 224, 165,
         128, 224, 164, 130, 32, 224, 164, 170, 224, 164, 185, 224, 165, 129, 224, 164, 130, 224, 164,
         154, 224, 164, 164, 224, 165, 128);
      Runes_Iterator : UCA.Unicode_Reference      := (Container => Runes'Access);
      Hindi_Iterator : UCA.Unicode_Reference      := (Container => Hindi'Access);

      function Convert is new Ada.Unchecked_Conversion (Source => Code_Points, Target => Wide_Wide_Character);
      -- function Convert is new Ada.Unchecked_Conversion (Source => String, Target => Unicode_String);

      function "=" (Left : UCA.Unicode_String; Right : String) return Boolean is
      begin
         if Left'Length /= Right'Length then
            return False;
         else
            for I in Left'Range loop
               if Left (I) /= Octets (Character'Pos (Right (I))) then
                  return False;
               end if;
            end loop;
         end if;

         return True;
      end "=";

      Error : constant String := "Compiler converted UTF-8 does not equal Unicode_String.";
   begin
      -- for Code in Runes_Iterator.Iterate loop
      --    Put (Convert (UCA.Element (Code)));
      -- end loop;

      -- New_Line;

      Ahven.Assert (Condition => Runes = "ᚠᛇᚻ᛫ᛒᛦᚦ᛫ᚠᚱᚩᚠᚢᚱ᛫ᚠᛁᚱᚪ᛫ᚷᛖᚻᚹᛦᛚᚳᚢᛗ",
                    Message   => "Runes: " & Error);

      -- for Code of Hindi_Iterator loop
      --    Put (Convert (Code));
      -- end loop;

      -- New_Line;

      Ahven.Assert (Condition => Hindi = "मैं काँच खा सकता हूँ और मुझे उससे कोई चोट नहीं पहुंचती",
                    Message   => "Hindi: " & Error);
   end Test_Iterator;
end UCA.Tests;
