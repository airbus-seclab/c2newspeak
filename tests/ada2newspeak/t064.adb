-- diverses utilisations de constantes
procedure T064 is
   type Enum is (E1,E2,E3);
   type Entier is new Integer;
   A : constant Float := 3.5;
   B : constant := A;
   C : constant Enum := E2;
   D : constant Entier := 5;
   E : constant := D;

   X : Entier;
begin
   X := E;
end T064;
