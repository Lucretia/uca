------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020-2021, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
-- with Ada.Text_IO;
with Ada.Sequential_IO;

package UCA.IO.Memory is
   type Memory_File_Type (Length : Positive) is
      record
         Data : Unicode_String (1 .. Length) := (others => Octets'First);
      end record;

   package Octet_IO is new Ada.Sequential_IO (Element_Type => Octets);

   procedure Open (File : in out Octet_IO.File_Type;
                   Name : in String);

   function Get (File : in Octet_IO.File_Type) return Memory_File_Type;
end UCA.IO.Memory;
