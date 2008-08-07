-- opérations sur les integers
procedure t005 (X : in out Integer; Y : out Boolean) is
begin
  X := -1*2*X;
  X := -1+2+(-3);

  X := -1*2+(-3)-4/2;

  X := -1/2*3 + (5 rem 2) - (-3)*(5 rem 4);
  Y := X < 5;
  Y := X = 5;
  Y := 2 >= X;
  Y := 2 /= 3;
  Y := 6 > X;
  Y := X <= 2*3;
end t005;
