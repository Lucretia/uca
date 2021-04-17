------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020-2021, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

package body UCA.IO.Memory is
   procedure Open (File : in out File_Type;
                   Mode : in File_Mode;
                   Name : in String;
                   Form : in String := "") is
   begin
      if Mode /= In_File then
         raise Mode_Error with "Memory files only allow input at this time.";
      end if;

      Open (File, Mode, Name, Form);
   end Open;

   function Get (File : in File_Type) return Memory_File_Type is
      pragma Warnings (Off);
      function Convert is new Ada.Unchecked_Conversion
         (Source => File_Size, Target => Positive);
      pragma Warnings (On);

      File_Length : File_Size := Size (Name (File));
      Str         : String (1 .. Convert (File_Length));

      subtype Ranged_String is String (1 .. Convert (File_Length));
      subtype Ranged_UString is Unicode_String (1 .. Convert (File_Length));

      function Convert is new Ada.Unchecked_Conversion
         (Source => Ranged_String, Target => Ranged_UString);
   begin
      Get (File, Str);

      return F : Memory_File_Type (Convert (File_Length)) do
         F.Data := Convert (Str);
      end return;
   end Get;
end UCA.IO.Memory;
