package body t104 is

   function A return Integer is
   begin
      return 12;
   end A;


   procedure Main is
      Z : Integer range 0..12;
   begin
      Z := A;
   end Main;

end t104;
