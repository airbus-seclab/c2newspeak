-- Assignment of loop parameter.
--
-- 3.3.(19) : " The following (and no others) represent constants :
--              [...] a loop parameter [...] "
--
-- Author : Etienne Millon
-- Date   : Tue Jul  7 2009
--
procedure t304 is
begin
  for I in 1 .. 5 loop
    I := 2;
  end loop;
end t304 ;
