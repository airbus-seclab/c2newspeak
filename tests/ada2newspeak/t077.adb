-- import de constante
with T077a; use T077a;
package body T077 is

   procedure Main is

      X : constant := A;
      Y : Integer;
   begin
      Y := X;
   end Main;

end T077;
