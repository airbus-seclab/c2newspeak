procedure T128 is
   type Enum is (A, B, C);
   for Enum use (A => 1, B => 2, C => 3);
   X : Enum := B;
begin
   X := C;
end T128;

