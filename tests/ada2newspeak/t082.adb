-- import de littéral d'énumération
with T082a; use T082a;
package body T082 is

   C : constant Enum := B;

   procedure Main is
      X : constant Enum := C;
      Y : Enum;
      B : constant Enum := T082a.C;

   begin
      Y := A;
   end Main;

end T082;
