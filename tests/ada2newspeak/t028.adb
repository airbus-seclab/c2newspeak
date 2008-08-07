procedure T028 is
   X : Integer := 0;
begin
   loop
      if X <100 then
         X := X+1;
      else
         X := X - 100;
      end if;
   end loop;
end T028;
