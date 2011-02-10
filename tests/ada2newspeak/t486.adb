procedure T486 is
   subtype T_2 is Integer range 1 .. 2;
   type T_P is record
      Y : Integer;
      Z : Integer;
   end record;
   type T_T is array ( T_2 ) of T_P ;
   type T_Q is record
      Y : Integer;
      TR  : T_T;
   end record;
   type T is record
      C       : Integer;
      P           : T_Q;
   end record;
   A  : T;
   R  : Integer;
begin
   R := A.P.TR(2).Z;
end T486;
