procedure t412 is
   type I is ( T1, T2);
   type T_C is
      record
         F : Integer;
      end record;
   Y : Integer;
   type T_T is array (I) of T_C;

   Ci:constant T_T :=((F => 5), (F => 8));

begin
   Y := Ci(T1).F;

end t412;
