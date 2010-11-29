procedure t419 is
   type X is (A, B , C);
   for X use (A => 5, B => 10, C => 15);

 --  subtype Y is X range A..B;

   DATA : X ;

begin
   for Z in X loop
      if DATA = A then
        DATA := Z;
      end if;
   end loop;
end t419;
