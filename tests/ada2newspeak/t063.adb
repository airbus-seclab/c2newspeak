-- diverses déclarations constantes
procedure T063 is
   type Enum is (E1,E2,E3);
   type Entier is new Integer;
   A : constant Float := 3.5;
   B : constant := 5.6;
   C : constant Enum := E2;
   D : constant Entier := 5;
begin
   null;
end T063;
