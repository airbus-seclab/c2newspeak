-- déclaration de sous-type et utilisation
procedure T067 is
   subtype T is Integer range 0..100;
   subtype T2 is T range 0..50;
   X : T := 5;
   Y : T2 := 10;
begin
   X := X + 20 - 2*Y;
end T067;
