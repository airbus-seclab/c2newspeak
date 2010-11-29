--with Text_IO.Enumeration_IO;

Procedure t420 is
   type X is (A, B , C);
   for X use (A => 5, B => 10, C => 15);

   subtype Y is X range A..B;

   DATA : X ;
   RES  : Y;
begin
   for Z in Y loop
      --     Text_IO.Put(Z);
     if DATA = A then


        DATA := Z;
      end if;
   end loop;

end t420;
