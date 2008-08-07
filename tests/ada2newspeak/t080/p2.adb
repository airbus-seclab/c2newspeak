-- import de constante, masquée par local
with P1; use P1;
package body P2 is

   function A return Integer is
   begin
      return 5;
   end A;

   procedure Main is
      A : constant := 0;
      X : constant := A;
      Y : Integer;
   begin
      Y := X;
   end Main;

end P2;
