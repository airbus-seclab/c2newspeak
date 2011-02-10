procedure T490 is
   subtype T_2 is Integer range 1 .. 2 ;
   type T_TT is array ( T_2) of Integer;
   type T_T is record
      AAA : Integer;
      BBB  : T_TT;
   end record;
   type T_Q is record
      Y : Integer;
      TR  : T_T;
   end record;
   type T is record
      C       : Integer;
      P           : T_Q;
   end record;
   A  : T;
   R  :  Integer;
 begin
   R := A.P.TR.BBB(1);
end T490;
