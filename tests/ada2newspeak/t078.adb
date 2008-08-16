-- import de constante, masquée par local
with T078a; use T078a;
package body T078 is

   A : constant := 0;

   procedure Main is

      X : constant := A;
      Y : Integer;
   begin
      Y := X;
   end Main;

end T078;
