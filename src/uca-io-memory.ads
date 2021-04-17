------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020-2021, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ada.Text_IO;

package UCA.IO.Memory is
   type Memory_File_Type (Length : Positive) is
      record
         Data : Unicode_String (1 .. Length);
      end record;

   procedure Open (File : in out Ada.Text_IO.File_Type;
                   Mode : in Ada.Text_IO.File_Mode;
                   Name : in String;
                   Form : in String := "");

   function Get (File : in Ada.Text_IO.File_Type) return Memory_File_Type;
end UCA.IO.Memory;
