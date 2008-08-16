-- import de constante, masquée par local
with T080a; use T080a;
package body T080 is

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

end T080;
