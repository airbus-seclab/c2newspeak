procedure t417 is
   type X is (A, B , C);
   subtype Y is X range A..B;
   DATA : X ;
   RES  : Y;
begin
   for Z in Y loop
      if DATA = A then
        DATA := Z;
      end if;
   end loop;
end t417;
