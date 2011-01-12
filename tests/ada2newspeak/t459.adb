procedure T459 is
subtype GE is Integer range 1 .. 2;
type E is record
   B0: Integer ;
   B1:  Integer ;
end record;
type D is record
   U : E;
   L : E;
end record ;
type T_E is  array ( GE ) of D;
R : Integer;
M :T_E ;
begin
   R := M(1).L.B1;
end T459;

