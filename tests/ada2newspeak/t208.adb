procedure T208 is

   subtype Roo is Integer range 1..5;

   type T is array ( Roo ) of Integer;

   TT : T;
   Z  : Integer;

begin
   TT(2) := 4;
   Z := TT(6);
   null;

end T208;
