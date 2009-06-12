-- Loop statement with an identifier.
--
-- When a loop has an identifier, it should be present at the "end loop".
--
-- Author : Etienne Millon
-- Date   : Mon Jun  8 2009
--
procedure t270 is
begin
    some_loop: loop
      null;
    end loop;
end t270;
