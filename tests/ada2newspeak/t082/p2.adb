-- import de littéral d'énumération
with P1; use P1;
package body P2 is

   C : constant Enum := B;

   procedure Main is
      X : constant Enum := C;
      Y : Enum;
      B : constant Enum := P1.C;

   begin
      Y := A;
   end Main;

end P2;
