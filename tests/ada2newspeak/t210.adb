package body T210 is

   function F(Y:Integer) return Integer is
   begin
      return 1;
   end F;

   procedure main is
      X : Integer;
   begin
      X := F(2);
   end main;

end T210;
