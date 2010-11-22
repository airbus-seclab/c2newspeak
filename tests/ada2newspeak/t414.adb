procedure t414 is
   type I is ( T1, T2);
   type T_C is
      record
         F : Integer;
      end record;

   type T_T is array (I) of T_C;

   Ci:constant T_T :=((F => 5), (F => 8));

begin
   Ci(T1).F := 5;

end t414;
