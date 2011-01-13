package body  T462 is
   R  : Integer;
   X  : GE := 1;
   Mi : T_E ;
   RE : E ;
   function M (Y : in GE) Return E  is
   begin
      return ( Mi(Y) );
   end M;
    procedure A is
    begin
        M(X).B1 := R;
    end A;
end T462;

