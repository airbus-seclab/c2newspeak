-- Loop statement with an identifier.
--
-- When a loop has an identifier at the end,
-- it should be present at the beginning.
--
-- Author : Etienne Millon
-- Date   : Mon Jun  8 2009
--
procedure t271 is
begin
    loop
      null;
    end loop some_loop;
end t271;
