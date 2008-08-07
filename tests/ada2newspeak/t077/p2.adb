-- import de constante
with P1; use P1;
package body P2 is

   procedure Main is

      X : constant := A;
      Y : Integer;
   begin
      Y := X;
   end Main;

end P2;
