------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ahven.Framework;

package UCA.Tests is
   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);
private
   procedure Test_Octets;
end UCA.Tests;
