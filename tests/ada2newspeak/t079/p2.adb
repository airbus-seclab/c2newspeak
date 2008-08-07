-- import de constante, masquée par local, utilisation d'un sélecteur
with P1; use P1;
package body P2 is

   A : constant := 0;

   procedure Main is
      X : constant := P1.A;
      Y : Integer;
   begin
      Y := X;
   end Main;

end P2;
