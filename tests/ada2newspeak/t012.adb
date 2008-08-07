--test if
procedure t012 is
   X : Integer := 3;
begin
   if X >2
   then
      X := X+1;
      X := X+4;
      if (X <10)
      then
         X := X*2;
      elsif (X =42)
      then
         return;
      elsif X<26
      then
         X := 42;
      else
         X := X-1;
      end if;
   end if;
end t012;

