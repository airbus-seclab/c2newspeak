-- variable A masque la fonction de t050; mais masqué par le type interne T
with T095; use T095;
package body T050 is

   A : Integer :=0;
   X : Boolean;

   procedure Main is
      type T is (A,B,C);
   begin
      X := A;
   end Main;
end T050;
