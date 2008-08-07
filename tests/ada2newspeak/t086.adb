-- test sur les sous-types flottants statique
procedure t086 is

   subtype Sfloat is Float range 10.0..50.2;
   subtype Sfloat2 is Sfloat range 10.2..45.3;

   X : Sfloat := 12.5;
   X2 : Sfloat range 12.5..25.2 := 20.0;

   A : constant Sfloat := 32.3;
   B : constant := 22.5;

   type DFloat is new SFloat range B..30.5;

begin
   X := Sfloat2'(15.4);
   X2 := 20.225;
end T086;
