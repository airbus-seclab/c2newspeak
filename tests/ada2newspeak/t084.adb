procedure T084 is
   A : constant Integer := 3;
   B : constant := 5;
   C : constant := 7;

   I : constant Boolean := +A=B;
   J : constant Boolean := A+B=C;

   type T1 is (X,Y,Z);

   F : constant Boolean := X=Y;

begin
   null;

end T084;
