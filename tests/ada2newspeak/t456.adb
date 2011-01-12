procedure T456 is
   type T32  is range 0 .. 2**31 - 1;
   type T16  is range 0 .. 2**2 - 1;
   CO : T16  ;
   DE : T32 := 0;
begin
   DE := T32 (CO);
end T456;
