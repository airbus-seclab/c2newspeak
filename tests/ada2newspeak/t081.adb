-- import de nombre avec sélecteur
-- appel de fonction dans constante (donc non-statique)
with T081a; use T081a;
package body T081 is

   function A return Integer is
   begin
      return 5;
   end A;

   C : Integer;

   procedure Main is
      X : constant Integer := A;
      Y : Integer;
      B : constant := T081a.C;

   begin
      Y := X+T081a.C+C+B;
   end Main;

end T081;
