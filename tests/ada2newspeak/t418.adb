procedure t418 is
   type X is (A, B, C);
   subtype Y is X range A..B;
   DATA : X ;
   RES  : Y;
begin
   DATA := RES;
end t418;
