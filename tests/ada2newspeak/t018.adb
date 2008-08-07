--definition de types dépendants
procedure t018 is
   type Enum1 is (A1,A2,A3,A4,A5,A6,A7,A8);
   type AliasEnum1 is new Enum1;
   type Alias2 is new AliasEnum1;
   X1 : Enum1 := A2;
   X2 : AliasEnum1 := A7;
   X3 : Alias2 := A4;
begin
   if A1=X1 then
      null;
   elsif A3 = X2 then
      null;
   elsif A5 = X3 then
     null;
   end if;

end t018;
