procedure T122 is

   type T1 is (X,Y,Z);
   type T2 is new T1;

   B : constant Boolean := X=Y;

begin
   null;

end T122;
