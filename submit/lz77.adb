with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Big_Integers;        use Big_Integers;

-- Submission authored by:
-- 1432166 Weizhao Li
-- 1484354 Chang Liu

-- This file requires Proof Level to be set to: 1

package body LZ77 with
  SPARK_Mode
is
   function Length_Acc (Input : in Token_Array) return Partial_Length is
      Result : Partial_Length (Input'Range) := (others => Zero);
   begin

      for Index in Input'Range loop
         -- Note this loop invariant needs "Proof level = 1" to prove it
         pragma Loop_Invariant
           ((for all I in Input'First .. Index - 1 =>
               Result (I) =
               (if I = Input'First then Zero else Result (I - 1)) +
                 To_Big_Integer (Input (I).Length) + To_Big_Integer (One))
            and then
            (for all I in Input'First .. Index - 1 =>
               (for all J in Input'First .. I - 1 =>
                  Result (I) > Result (J))));
         Result (Index) :=
           (if Index = Input'First then Zero else Result (Index - 1)) +
           To_Big_Integer (Input (Index).Length) + To_Big_Integer (One);
      end loop;
      return Result;
   end Length_Acc;

   procedure Put (T : in Token) is
   begin
      Put ("Offset: ");
      Put (T.Offset);
      New_Line;
      Put ("Length: ");
      Put (T.Length);
      New_Line;
      Put ("Next_C: ");
      Put (T.Next_C);
      New_Line;
   end Put;

   -- task3:
   -- 1. Compared to languages like C, Ada provides stricter type checking.
   -- The compiler automatically checks for certain types of errors,
   -- making Ada more suitable for implementation in some cases, especially
   -- where there might be bugs in C implementations.
   -- 2. To complete task2, I added many additional boundary checks and Loop_Invariants
   -- to ensure the correctness of the program. These checks provide many
   -- additional guarantees against array bounds and integer overflow errors.
   -- These checks are crucial when you cannot control possible inputs.
   -- In such cases, you should use the proven safe Ada implementation
   -- rather than a potentially buggy Ada implementation.

   procedure Decode
     (Input         : in     Token_Array; Output : in out Byte_Array;
      Output_Length :    out Natural; Error : out Boolean)
   is
      Output_Index : Natural;
   begin
      -- IMPLEMENT THIS
      -- Check if the output array's first index is less than 0 (impossible case for Ada arrays)
      if Output'First < 0 then
         Error         := True;
         Output_Length := 0;
         return;
      end if;
      -- Initialize output length and index, and set error flag to false
      Output_Length := 0;
      Output_Index  := Output'First;
      Error         := False;
      -- Iterate over each token in the input array
      for Token_Index in Input'Range loop
         declare
            Token_Offset : Natural   := Input (Token_Index).Offset;
            Token_Length : Natural   := Input (Token_Index).Length;
            Token_Next_C : Character := Input (Token_Index).Next_C;
         begin
            -- Check if the token's length will cause an overflow in the output index
            if Token_Length > Natural'Last - Output_Index then
               Error         := True;
               Output_Length := 0;
               return;
            end if;

            -- Check if the output index is valid and if there is enough space for the token
            if Output_Index < Token_Offset or
              Token_Length + Output_Index - 1 > Output'Last
            then
               Error         := True;
               Output_Length := 0;
               return;
            end if;
            -- Copy the characters specified by the token's offset and length
            for I in 1 .. Token_Length loop
               if (Output_Index - Token_Offset) < Output'First - (I - 1) then
                  Error         := True;
                  Output_Length := 0;
                  return;
               end if;
               Output (Output_Index + I - 1) :=
                 Output ((Output_Index - Token_Offset) + (I - 1));
            end loop;

            -- Check if there is enough space in the output for the next token
            if Output_Index > Output'Last - Token_Length - 1 then
               Error         := True;
               Output_Length := 0;
               return;
            end if;

            -- Check if the position to insert the next character is valid
            if Output_Index + Token_Length < Output'Last and
              Output_Index + Token_Length >= Output'First
            then
               Output (Output_Index + Token_Length) := Token_Next_C;
               Output_Index := Output_Index + Token_Length + 1;
            else
               Error         := True;
               Output_Length := 0;
               return;
            end if;
            -- Loop invariant to ensure the output index is within valid range
            pragma Loop_Invariant
              ((Output'First <= Output_Index - Output_Length));
         end;
      end loop;
      if Error then
         Output_Length := 0;
      else
         Output_Length := Output_Index - Output'First;
      end if;
   end Decode;

   function Is_Valid (Input : in Token_Array) return Boolean is
      Decoded_Length : Natural := 0;
   begin
      -- Iterate over each token in the input array
      for Index in Input'Range loop
         declare
            Token_Offset : Natural   := Input (Index).Offset;
            Token_Length : Natural   := Input (Index).Length;
            Next_C       : Character := Input (Index).Next_C;
         begin
            -- Check if any of the invalid conditions are met
            if
               -- If the token has a positive length, offset is zero, and next character is not null

              (Token_Length > 0 and then Token_Offset = 0
               and then Next_C /= Character'Val (0))
               -- If the token offset is greater than the decoded length

              or else (Token_Offset > Decoded_Length)
               -- If adding the token length to the decoded length would cause overflow

              or else (Integer'Last - Decoded_Length - 1 < Token_Length)
            then
               return False;
            end if;
            -- Update the decoded length by adding the token length plus one for the next character
            Decoded_Length := Decoded_Length + Token_Length + 1;
         end;
      end loop;
      return True;
   end Is_Valid;

   --  Task6 :
   --  1. Compared to Section 2.1, the implementation of Decode_Fast is more efficient
   --  because it does not have to check for the validity of the output index and
   --  the space in the output array for each token. This reduces the number of
   --  checks and improves the performance of the implementation.
   --  2.If I added these checks to the Decode_Fast implementation, it would be
   --  similar to the Decode implementation and would not be as efficient as it
   --  is now. Actrually, we have already added these checks in is_valid function.
   --  Too many checks are meaningless and will reduce the efficiency of the program.

   procedure Decode_Fast
     (Input         : in     Token_Array; Output : in out Byte_Array;
      Output_Length :    out Natural)
   is
      Output_Index :
        Integer; -- Index to track the position in the output array
   begin
      -- IMPLEMENT THIS
      Output_Length := 0; -- Initialize the output length
      Output_Index  :=
        Output'First; -- Initialize the output index to the first position
      for Token_Index in Input'Range loop
         declare
            Token_Offset : Natural   :=
              Input (Token_Index).Offset; -- Offset of the current token
            Token_Length : Natural   :=
              Input (Token_Index).Length; -- Length of the current token
            Token_Next_C : Character :=
              Input (Token_Index).Next_C; -- Next character after the token
         begin
            -- Copy characters specified by the token's offset and length
            for I in 1 .. Token_Length loop
               Output (Output_Index + I - 1) :=
                 Output ((Output_Index - Token_Offset) + (I - 1));
            end loop;
            -- Loop invariant to ensure the output index is within valid range
            pragma Loop_Invariant
              ((Output'First <= Output_Index - Output_Length));
            -- Insert the next character after the copied characters
            Output (Output_Index + Token_Length) := Token_Next_C;
            Output_Index := Output_Index + Token_Length + 1;
         end;
      end loop;
      Output_Length := Output_Index - Output'First;
   end Decode_Fast;
end LZ77;
