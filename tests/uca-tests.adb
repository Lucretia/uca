------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ada.Characters.Latin_1;

package body UCA.Tests is
   package L1 renames Ada.Characters.Latin_1;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "UCA.Tests");

      Ahven.Framework.Add_Test_Routine (T, Test_Octets'Access, "Octets");
   end Initialize;

   procedure Test_Octets is
      A   : constant Octets := Character'Pos ('A');    --  <= 7F
      STS : constant Octets := Character'Pos (L1.STS); --  >= 7F
   begin
      Ahven.Assert (Condition => (A and Octet_Mask_One) = A,
                    Message   => "A is not an octet and should be");

      Ahven.Assert (Condition => (STS and Octet_Mask_One) /= STS,
                    Message   => "STS is an octet and should not be");
   end Test_Octets;
end UCA.Tests;
