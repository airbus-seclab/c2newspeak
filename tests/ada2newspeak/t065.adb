-- calcul d'expression statique, utilisation statique
procedure T065 is
   type Enum is (E1,E2,E3);

   --entier
   A : constant := 2*3+5-(1+3)/2*2- 3 mod 2;
   B : constant := (1+3)/2*2;
   C : constant := 3 rem 2;
   D : constant := B**3 - A - C;
   E : constant := -D;
   F : constant := abs E;

   --reel
   Ar : constant := 2.0*3.0+5.0-(1.0+3.0)/2.0*2.0- 3.0;
   Br : constant := (1.0+3.0)/2.0*2.0;
   Cr : constant := 1.2**5;
   Dr : constant := Br**3 - Ar - Cr;
   Er : constant := -Dr;
   Fr : constant := abs Er;

   -- utilisation dans un range
   type Mes_Entiers is range C..D;

   --enum, booléen
   Ae : constant Enum := E2;
   Be : constant Enum := Ae;

   Ab : constant Boolean := Ae = E3;
   Bb : constant Boolean := Ae /= Be;
   Cb : constant Boolean := A > B or else (Ar <= Br and then Er >= Fr);


begin
   null;
end T065;
