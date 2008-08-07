-- test sur les sous-types énumératif statique
procedure t088 is

   type Enum is (A,B,C,D,E,F,G,H);

   subtype S1 is Enum range B..G;
   subtype S2 is S1 range C..F;

   X : S1 := D;
   X2 : S1 range C..F := E;

   C1 : constant S1 := F;

   type Derive is new S1 range D..C1;

   Y : Derive := E;

begin
   X := S2'(C);
   X2 := D;
end T088;
