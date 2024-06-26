with LZ77; use LZ77;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Text_IO;
use Ada.Integer_Text_IO;

procedure Main with
  SPARK_Mode
is
   -- a trivial token sequence that encodes the string "AAAB"
   T1 : Token_Array (1 .. 2) :=
     (2 => (Offset => 0, Length => 0, Next_C => 'A'),
      3 => (Offset => 1, Length => 2, Next_C => 'B'));
   --  其他测试task1的例子
   --  ❌
   T2 : Token_Array (1 .. 2) :=
     (2 => (Offset => 0, Length => 0, Next_C => 'A'),
      3 => (Offset => 2, Length => 1, Next_C => 'B'));
   --  ✅ XXAXB
   T3 : Token_Array (1 .. 2) :=
     (2 => (Offset => 0, Length => 2, Next_C => 'A'),
      3 => (Offset => 0, Length => 1, Next_C => 'B'));
   --

   Error : Boolean;
   B     : Byte_Array (1 .. 100) := (others => 'X');
   BLen  : Natural;
begin
   Decode (T1, B, BLen, Error);
   --  Decode (T2, B, BLen, Error);
   --  Decode (T3, B, BLen, Error);
   if not Error then
      Put ("No erorr reported. Got this many bytes: ");
      Put (BLen);
      New_Line;
      if BLen <= B'Length then
         for Index in B'First .. B'First + BLen - 1 loop
            Put (Item => B (Index));
         end loop;
         New_Line;
      else
         Put ("Indicated decompressed length must be wrong!");
         New_Line;
      end if;
   end if;

   if Is_Valid (T1) then
      --  test
      Put ("T1 is valid");
      New_Line;
      --  test
      Decode_Fast (T1, B, BLen);
      Put ("Got this many: ");
      Put (BLen);
      New_Line;
      pragma Assert (BLen = 4);
      for Index in B'First .. B'First + BLen - 1 loop
         Put (Item => B (Index));
      end loop;
      New_Line;
   end if;
end Main;
