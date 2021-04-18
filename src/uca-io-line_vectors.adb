------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020-2021, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ada.Characters.Latin_1;

package body UCA.IO.Line_Vectors is
   function Convert (File             : in Memory.Memory_File_Type;
                     Retain_New_Lines : in Boolean := False) return Line_Vector.Vector is
      Current    : Positive := Positive'First;
      Next_First : Positive := Positive'First;

      Result : Line_Vector.Vector;

      package L1 renames Ada.Characters.Latin_1;
   begin
      while Current <= File.Data'Length loop
         --  TODO: Fix for any new line in unicode.
         if File.Data (Current) = Character'Pos (L1.LF) then
            --  Skip blank lines?
            if Current = Next_First and Retain_New_Lines then
               Result.Append (New_Line'(others => <>));
            elsif Current - Next_First > 1 then
               Result.Append (Text_Line'(Length => Current - Next_First,
                                         Text   => File.Data (Next_First .. Current - 1)));
            end if;

            --  Is this the EOF?
            if Current < File.Data'Last then
               Next_First := Current + 1;  --  Need to skip past the NL which the increment below will also do.
            end if;
         end if;

         --  Skip the new line.
         Current := Current + 1;
      end loop;

      return Result;
   end Convert;
end UCA.IO.Line_Vectors;
