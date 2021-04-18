------------------------------------------------------------------------------------------------------------------------
--  Copyright © 2020-2021, Luke A. Guest
--
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------------------------------------------------
with Ada.Containers.Indefinite_Vectors;
with UCA.IO.Memory;

package UCA.IO.Line_Vectors is
   --  A file organised as an array of lines.
   --
   --  This is a useful structure for parsing file formats which are ad-hoc, like markdown.
   type Root_Line is abstract tagged null record;

   type New_Line is new Root_Line with null record;

   type Text_Line (Length : Positive) is new Root_Line with record
      Text : Unicode_String (1 .. Length);
   end record;

   package Line_Vector is new Ada.Containers.Indefinite_Vectors
      (Index_Type   => Natural,
       Element_Type => Root_Line'Class);

   function Convert (File             : in Memory.Memory_File_Type;
                     Retain_New_Lines : in Boolean := False) return Line_Vector.Vector;
end UCA.IO.Line_Vectors;
