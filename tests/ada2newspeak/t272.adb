-- Loop statement with a beginning identifier and a different end identifier.
--
-- Author : Etienne Millon
-- Date   : Mon Jun  8 2009
--
procedure t272 is
begin
    some_loop: loop
      null;
    end loop some_other_loop;
end t272;
