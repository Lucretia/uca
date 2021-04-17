------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ada.Text_IO;

package body UCA.IO is
   procedure Put (Item : in Unicode_String) is
      subtype Ranged_String is String (Item'Range);
      subtype Ranged_UString is Unicode_String (1 .. Item'Last);

      function Convert is new Ada.Unchecked_Conversion
         (Source => Ranged_UString, Target => Ranged_String);
   begin
      Ada.Text_IO.Put (Convert (Item));
   end Put;


   procedure Put_Line (Item : in Unicode_String) is
      subtype Ranged_String is String (Item'Range);
      subtype Ranged_UString is Unicode_String (1 .. Item'Last);

      function Convert is new Ada.Unchecked_Conversion
         (Source => Ranged_UString, Target => Ranged_String);
   begin
      Ada.Text_IO.Put (Convert (Item));
      Ada.Text_IO.New_Line;
   end Put_Line;
end UCA.IO;
