-- déclaration de sous-type et utilisation
procedure T066 is
   subtype T is Integer range 0..100;
   X : T := 5;
begin
   X := X + 10 - 5 - 2 - 3;
end T066;
