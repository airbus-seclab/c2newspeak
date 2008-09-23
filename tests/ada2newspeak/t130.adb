procedure T130 is
   type Enum is (A, B, C);

   for Enum use (A => 0, B => 1, C => 127);
   X : Enum := C;
begin
   X := B;
   X := A;
end T130;
