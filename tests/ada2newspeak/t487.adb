with T487a;
use T487a;

  procedure T487 is
   subtype T_2 is Integer range 1 .. 2 ;
   type T_T is array ( T_2, T_2 ) of Integer;
   type T_Q is record
      Y : Integer;
      TR  : T_T;
   end record;
   type T is record
      C       : Integer;
      P           : T_Q;
   end record;
   A  : T;
   R  : T487a.Inti;
begin
   R := T487a.Inti (A.P.TR(1, 1));
end T487;
