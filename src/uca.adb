with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces;        use Interfaces;

package body UCA is
   package Natural_IO is new Integer_IO (Num => Natural);
   use Natural_IO;

   package Octet_IO is new Modular_IO (Num => Octets);
   use Octet_IO;

   function Has_Element (Position : in Unicode_Cursor) return Boolean is
   begin
      return (Position /= No_Element);
   end Has_Element;

   function Iterate (Container : in Unicode_Reference) return Unicode_Iterators.Forward_Iterator'Class is
   begin
      return Unicode_Iterator'(Container => Container.Container);
   end Iterate;

   function Element_Value (Container : in Unicode_Reference; Position : in Unicode_Cursor) return Code_Points is
      pragma Unreferenced (Container);
   begin
      return Position.Code;
   end Element_Value;

   function Element (Position : in Unicode_Cursor) return Code_Points is
   begin
      return Position.Code;
   end Element;

   function Parse_UTF_8 (First : in Octets) return Code_Points is
   begin
      return Code_Points (First);
   end Parse_UTF_8;

   function Parse_UTF_8 (First, Second : in Octets) return Code_Points is
   begin
      return Code_Points (Shift_Left (Unsigned_32 (First and Octet_Data_Mask_Two), 6) or
                          Unsigned_32 (Second and Octet_Data_Mask_Follower));
   end Parse_UTF_8;

   function Parse_UTF_8 (First, Second, Third : in Octets) return Code_Points is
   begin
      return Code_Points (Shift_Left (Unsigned_32 (First and Octet_Data_Mask_Three), 12) or
                          Shift_Left (Unsigned_32 (Second and Octet_Data_Mask_Follower), 6) or
                          Unsigned_32 (Third and Octet_Data_Mask_Follower));
   end Parse_UTF_8;

   function Parse_UTF_8 (First, Second, Third, Fourth : in Octets) return Code_Points is
   begin
      return Code_Points (Shift_Left (Unsigned_32 (First and Octet_Data_Mask_Four), 18) or
                          Shift_Left (Unsigned_32 (Second and Octet_Data_Mask_Follower), 12) or
                          Shift_Left (Unsigned_32 (Third and Octet_Data_Mask_Follower), 6) or
                          Unsigned_32 (Fourth and Octet_Data_Mask_Follower));
   end Parse_UTF_8;

   function Parse_UTF_8 (At_Position : in Unicode_Cursor) return Unicode_Cursor is
      Current_Index      : Natural     := At_Position.Index;
      Next_Element_Index : Natural     := Natural'First;
      Code               : Code_Points := 0;
      First              : Octets      := At_Position.Container (Current_Index); --  This should always be the first
                                                                                 --  element in a UTF-8 sequence.
   begin
      if (First and Octet_Follower_Mask) = Octet_Follower then
         declare
            Data     : String (1 .. 20);
            Position : String (1 .. 10);
         begin
            Put (To => Data,     Item => First, Base => 2);
            Put (To => Position, Item => Current_Index);

            raise Unicode_Parse_Error with
               "Octet (Index => " & Trim (Position, Ada.Strings.Both) &
               ", Data => " & Trim (Data, Ada.Strings.Both) &
               ") is a UTF-8 follower value, it should be a leading value.";
         end;
      end if;

      --  Parse the UTF-8 depending on the leading octet.
      if (First and Octet_Header_Mask_One) = Octet_Header_One then
         Code := Parse_UTF_8 (First);

         Next_Element_Index := Current_Index + 1;
      elsif (First and Octet_Header_Mask_Two) = Octet_Header_Two then
         Code := Parse_UTF_8 (First,
                              At_Position.Container (Current_Index + 1));

         Next_Element_Index := Current_Index + 2;
      elsif (First and Octet_Header_Mask_Three) = Octet_Header_Three then
         Code := Parse_UTF_8 (First,
                              At_Position.Container (Current_Index + 1),
                              At_Position.Container (Current_Index + 2));

         Next_Element_Index := Current_Index + 3;
      elsif (First and Octet_Header_Mask_Four) = Octet_Header_Four then
         Code := Parse_UTF_8 (First,
                              At_Position.Container (Current_Index + 1),
                              At_Position.Container (Current_Index + 2),
                              At_Position.Container (Current_Index + 3));

         Next_Element_Index := Current_Index + 4;
      end if;

      return Unicode_Cursor'(Container  => At_Position.Container,
                             Index      => Current_Index,
                             Next_Index => Next_Element_Index,
                             Code       => Code);
   end Parse_UTF_8;

   overriding
   function First (Object : in Unicode_Iterator) return Unicode_Cursor is
   begin
      --  Make sure it's not an empty string.
      if Object.Container'Length = 0 then
         return No_Element;
      end if;

      --  Pass ina  dummy first position cursor.
      return Parse_UTF_8 (Unicode_Cursor'(Container  => Object.Container,
                                          Index      => 1,
                                          Next_Index => Natural'First,
                                          Code       => 0));
   end First;

   overriding
   function Next (Object : in Unicode_Iterator; Position : in Unicode_Cursor) return Unicode_Cursor is
      -- pragma Unreferenced (Object);
   begin
      --  Make sure we can stop.
      if Position.Next_Index >= Position.Container'Last then
         return No_Element;
      end if;

      --  Update the index from the previous step.
      return Parse_UTF_8 (Unicode_Cursor'(Container  => Object.Container,
                                          Index      => Position.Next_Index,
                                          Next_Index => Position.Next_Index,
                                          Code       => 0));
   end Next;
end UCA;