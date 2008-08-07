-- expression static avec expression qualifiée
procedure t019 is
   type Entier is range Integer'(-32)..32;
   X : Entier := 5;
begin
   X := 3;

end t019;

