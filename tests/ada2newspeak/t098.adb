with T097;
use T097;
with T096;
with T099;
package body T098 is
   function t096 return Integer is
   begin
      return 5;
   end t096;

   function G return T097.Enum is
   begin
      return T097.G;
   end G;

   use T097;
   procedure Main (Z:Enum) is
      use T097;
      X : Integer;
   begin
      X := t096;
      t099(0);
   end Main;

   procedure Proc (Z : Integer) is
   begin
      Main(X);
   end Proc;

   function A return Integer is
   begin
     return 8;
   end;

end T098;
