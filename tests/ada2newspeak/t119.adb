package body t119 is

   function A return Integer is
   begin
      return 5;
   end A;

   procedure Main is
      X : constant Integer := A;
      Y : Integer;
      B : constant := X;

   begin
      Y := X;
   end Main;

end t119;
