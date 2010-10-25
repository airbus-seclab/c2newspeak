procedure t388 is
   type Y is
     record
        X        :  Integer ;
    end record;
   type M  is (M1, M2, M3);
   type A  is array ( M  ) of Y;
   Ci : constant  A := (M1 .. M3 => ( X => 0 ));

begin

   null;

end t388;
