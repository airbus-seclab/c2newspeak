procedure t136 is
   type E is (A,B,C);
   Z : constant E := A;
   type D is new E range Z..B;
   for D use (A=>10,B=>20,C=>30);
   Z2 : constant E := C;
   Z3 : constant D := B;
   X : D;
begin
   X := A;
end t136;
