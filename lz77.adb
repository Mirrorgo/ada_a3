with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

-- Submission authored by:
-- <INSERT YOUR NAMES HERE>

-- This file requires Proof Level to be set to: <INSERT HERE>

package body LZ77 with
  SPARK_Mode
is
   --  接受一个名为Input的Token_Array类型的输入参数，返回一个Partial_Length类型的结果
   function Length_Acc (Input : in Token_Array) return Partial_Length is
      --  这是一个局部变量的定义，变量名为Result，类型为Partial_Length，初始值为Zero
      Result : Partial_Length (Input'Range) := (others => Zero);
   begin

      --  Index是循环变量，Input'Range是循环的范围，表示从Input的第一个元素到最后一个元素
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

   procedure Decode
     (Input         : in     Token_Array; Output : in out Byte_Array;
      Output_Length :    out Natural; Error : out Boolean)
   is
      Output_Index : Natural;
   begin
      -- IMPLEMENT THIS
      if Output'First < 0 then
         Error         := True;
         Output_Length := 0;
         return;
      end if;
      Output_Index  := Output'First;  -- Index for the Output array
      Output_Length := 0;
      Error         := False;
      --      Output_Index + Input.Length - 1 <= Output'Last
      for Token_Index in Input'Range loop
         declare
            Token_Offset : Natural   := Input (Token_Index).Offset;
            Token_Length : Natural   := Input (Token_Index).Length;
            Token_Next_C : Character := Input (Token_Index).Next_C;
         begin

            --  if Output_Index - 1 < Output'First then
            --     -- 如果输出的起始索引小于 Output'First，则索引超出范围，报错
            --     Error         := True;
            --     Output_Length := 0;
            --     return;
            --  end if;

            if Output_Index < Token_Offset or
              Token_Length + Output_Index - 1 > Output'Last --边界对吗？
            then
               Error         := True;
               Output_Length := 0;
               return;
            end if;
            pragma Loop_Invariant
              (((Output_Index >= Token_Offset) and
                (Token_Length - 1 > Output'Last - Output_Index)) or
               (Error = True and Output_Length = 0));

            --  pragma Loop_Invariant
            --    ((if Token_Length >= 1 then
            --        (for all J in 1 .. Token_Length =>
            --           Output (Output_Index + J) =
            --           Output (Output_Index + J - Token_Offset))) or
            --     (Error = True and Output_Length = 0));
            for I in 1 .. Token_Length loop
               --  pragma Loop_Invariant
               --    (Output_Index - I + Token_Length + 1 = Output_Index);
               if (Output_Index - Token_Offset) < Output'First - (I - 1) then
                  Error         := True;
                  Output_Length := 0;
                  return;
               end if;
               Output (Output_Index + I - 1) :=
                 Output ((Output_Index - Token_Offset) + (I - 1));
            end loop;

            if Output_Index > Output'Last - Token_Length - 1 then
               --  if Output_Index + Token_Length + 1 > Output'Last then
               Error         := True;
               Output_Length := 0;
               return;
            end if;

            --  Output (Output_Index + Token_Length) := Token_Next_C;
            --  Output_Index := Output_Index + Token_Length + 1;

            if Output_Index + Token_Length < Output'Last and
              Output_Index + Token_Length >= Output'First -- 非常奇怪，感觉不用加的
            then
               Output (Output_Index + Token_Length) := Token_Next_C;
               Output_Index := Output_Index + Token_Length + 1;
            else
               Error         := True;
               Output_Length := 0;
               return;
            end if;
         end;
      end loop;

      -- Set the final output length
      Output_Length := Output_Index - Output'First;

   end Decode;

   function Is_Valid (Input : in Token_Array) return Boolean is
   begin
      -- IMPLEMENT THIS
      --  return False;
      -- 实现验证函数的逻辑，确保输入数据的有效性
      -- 这里我们假设输入是有效的，实际实现应根据需求修改
      return True;  -- 在实际实现中应提供正确的验证逻辑
   end Is_Valid;

   procedure Decode_Fast
     (Input         : in     Token_Array; Output : in out Byte_Array;
      Output_Length :    out Natural)
   is
   begin
      -- IMPLEMENT THIS
      Output_Length := 0;
   end Decode_Fast;

end LZ77;
