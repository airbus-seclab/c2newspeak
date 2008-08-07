-- import de constante, masquée par local
with P1; use P1;
package body P2 is

   A : constant := 0;

   procedure Main is

      X : constant := A;
      Y : Integer;
   begin
      Y := X;
   end Main;

end P2;
