package body  T460 is
   R : Integer;
   X : GE := 1;
   Mi : T_E ;
   function M (Y : in GE) Return D  is
   begin
      return ( Mi(Y) );
   end M;
    procedure A is
    begin
       --R := M(X).L.B0;
       R := M(X).L.B1;
    end A;
end T460;

