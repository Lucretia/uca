------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020-2021, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ada.Directories; use Ada.Directories;
-- with Ada.Text_IO;     use Ada.Text_IO;

package body UCA.IO.Memory is
   procedure Open (File : in out Octet_IO.File_Type;
                   Name : in String) is
   begin
      Octet_IO.Open (File => File, Name => Name, Mode => Octet_IO.In_File);
   end Open;

   function Get (File : in Octet_IO.File_Type) return Memory_File_Type is
      pragma Warnings (Off);
      function Convert is new Ada.Unchecked_Conversion (Source => File_Size, Target => Positive);
      pragma Warnings (On);

      File_Length : File_Size := Size (Octet_IO.Name (File));

      -- subtype Ranged_String is String (1 .. Convert (File_Length));
      -- subtype Ranged_UString is Unicode_String (1 .. Convert (File_Length));

      -- function Convert is new Ada.Unchecked_Conversion
      --    (Source => Ranged_String, Target => Ranged_UString);
      Octet : Octets;
      Index : Positive := Positive'First;
   begin
      return F : Memory_File_Type (Convert (File_Length)) do
         while not Octet_IO.End_Of_File (File) loop
            Octet_IO.Read (File => File, Item => Octet);

            F.Data (Index) := Octet;

            Index := Index + 1;
         end loop;
      end return;
   end Get;
end UCA.IO.Memory;
