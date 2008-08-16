with T097;
use T097;
with T096;
with T099;
package body T098 is
   function A return Integer is
   begin
      return 5;
   end A;

   function G return T097.Enum is
   begin
      return T097.G;
   end G;

   use T097;
   procedure Main (Z:Enum) is
      use T097;
      X : Integer;
   begin
      X := A;
      P(0);
   end Main;

   procedure Proc (Z : Integer) is
   begin
      Main(X);
   end Proc;

end T098;
