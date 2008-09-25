package body t137 is

   type T1 is range -16..16;
   type T2  is (A1, A2, A3, A4);
   type T is array (T2) of T1;

   procedure P is
   begin
      null;
   end P;

end T137;

