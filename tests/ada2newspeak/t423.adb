procedure t423 is
   type Enum is (A, B, C);
   for Enum use (A => 1,
                 B => 2,
                 C => 17);
   X : Enum := B;
begin
   X := C;
end t423;
