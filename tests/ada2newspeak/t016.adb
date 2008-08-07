--definition de types dépendants
procedure t016 is
   type Enum1 is (A1,A2,A3,A4,A5,A6,A7,A8);
   type AliasEnum1 is new Enum1;
   type Alias2 is new AliasEnum1;
   X : Alias2 := A4;
begin
   null;
end t016;
