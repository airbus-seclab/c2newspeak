-- somme des entiers de 1 à 10 (version 2)
procedure T027 is
   X : Integer := 10;
   Y : Integer := 0;
begin
   loop
      Y := Y + X;
      X := X-1;
      exit when X<0;
   end loop;
end T027;
