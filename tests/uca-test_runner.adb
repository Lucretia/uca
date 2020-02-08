------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ahven.Text_Runner;
with Ahven.Framework;
with UCA.Tests;

procedure UCA.Test_Runner is
   S : Ahven.Framework.Test_Suite := Ahven.Framework.Create_Suite ("All");
begin
   Ahven.Framework.Add_Test (S, new UCA.Tests.Test);

   Ahven.Text_Runner.Run (S);
end UCA.Test_Runner;
