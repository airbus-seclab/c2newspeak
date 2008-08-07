-- variable A masque le littéral d'enumération de t049a; mais masqué par le type interne T
with T049a; use T049a;
package body T049 is

   A : Integer :=0;
   X : T;

   procedure Main is
      type T is (A,B,C);
   begin
      X := A;
   end Main;
end T049;
