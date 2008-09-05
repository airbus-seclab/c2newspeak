-- import de constante, masquée par local
with t118a; use t118a;
package body t118 is

   function A return Integer is
   begin
      return 5;
   end A;

   procedure Main is
      X : constant := A;
      Y : Integer;
   begin
      Y := X;
   end Main;

end t118;
