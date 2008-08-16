-- import de constante, masquée par local, utilisation d'un sélecteur
with T079a; use T079a;
package body T079 is

   A : constant := 0;

   procedure Main is
      X : constant := T079a.A;
      Y : Integer;
   begin
      Y := X;
   end Main;

end T079;
