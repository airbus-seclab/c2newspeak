--
package body T206 is

function Id (X: in Integer) return Integer is
begin
   return  X;
end Id;

procedure main is
   Xn : Integer;
begin
   Xn := Id(2147483647);

   Xn := Id(2147483648);
   --equivalent to Xn := Add(2,5);
end main;

end T206;
