procedure T464 is
   subtype GE is Integer range 1 .. 2;
   RES : Integer;
   X  : GE := 1;
   type F is record
      B0: Integer ;
      B1:  Integer ;
   end record;
   type E is record
      C0: F ;
      C1: F ;
   end record;
   type D is record
      U : E;
      L : E;
   end record ;
   type T_E is  array ( GE ) of D;
   R : Integer;
   M :T_E ;
begin
    RES :=  M(X).U.C1.B0;
end T464;

