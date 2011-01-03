Procedure T435 is
   subtype R is Integer range 1..3;
   type A1 is array (R) of Integer;
   type A2 is array (R) of A1;
   type A3 is array (R) of A2;
   X3 : A3 := (others  => (others  => (others  => 0)));
   X2 : A2 := (others  => (others  => 0));
   X : A1 := (others  => 0);
begin

   null;


end t435;
