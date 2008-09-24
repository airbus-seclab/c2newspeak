procedure t134 is
   type E is (A,B,C);
   for E use (A=>10,B=>15,C=>25);
   type D is new E;
   for D use (A=>0,B=>1,C=>2);
   X : E;
   Y : D;
begin
   X := B;
   Y := C;
end t134;
