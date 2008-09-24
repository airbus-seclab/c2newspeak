procedure t132 is
   type E is (A,B,C);
   for E use (A=>10,B=>15,C=>25);
   type D is new E;

   X : E;
   Y : D;
begin
   X := B;
   Y := C;
end t132;
