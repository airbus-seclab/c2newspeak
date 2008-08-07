-- import de nombre avec sélecteur
-- appel de fonction dans constante (donc non-statique)
with P1; use P1;
package body P2 is

   function A return Integer is
   begin
      return 5;
   end A;

   C : Integer;

   procedure Main is
      X : constant Integer := A;
      Y : Integer;
      B : constant := P1.C;

   begin
      Y := X+P1.C+C+B;
   end Main;

end P2;
