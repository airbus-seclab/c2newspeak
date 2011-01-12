procedure T458 is
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
M :T_E ;
begin
   M :=(others =>( U =>(others => 0),
                   L =>(others => 1))
       )
     ;
end T458;

