procedure t133 is
   type E is (A,B,C);
   type D is new E;
   for E use (A=>10,B=>15,C=>25);
   X : E;
   Y : D;
begin
   X := B;
   Y := C;
end t133;
