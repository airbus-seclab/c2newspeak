-- test sur les sous-types entiers statique
procedure t087 is

   subtype S1 is Integer range 10..50;
   subtype S2 is S1 range 10..45;

   X : S1 := 12;
   X2 : S1 range 15..20 := 17;

   A : constant S1 := 32;
   B : constant := 22;

   type D is new S1 range B..30;
   type R is range B..200;

   Y : D := 25;
   Z : R := 25;

begin
   X := S2'(15);
   X2 := 20;
end T087;
