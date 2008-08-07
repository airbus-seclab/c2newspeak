with P1;
use P1;
with P;
with A;
package body P2 is
   function A return Integer is
   begin
      return 5;
   end A;

   function G return P1.Enum is
   begin
      return P1.G;
   end G;

   use P1;
   procedure Main (Z:Enum) is
      use P1;
      X : Integer;
   begin
      X := A;
      P(0);
   end Main;

   procedure Proc (Z : Integer) is
   begin
      Main(X);
   end Proc;

end P2;
