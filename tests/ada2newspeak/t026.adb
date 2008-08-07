--somme des entiers de 1 à 10 (version 1)
procedure T026 is
   X : Integer := 10;
   Y : Integer := 0;
begin
   while(X > 0) loop
      Y := Y + X;
      X := X-1;
   end loop;
end T026;
