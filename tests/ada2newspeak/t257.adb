-- A "with" clause referring to an undefined package shall trigger a pretty
-- error message, not an uncaught-exception-or-whatever.
--
-- Author : Etienne Millon
-- Date   : Wed May  6 2009
--
with nonexistingpackage;
procedure t257 is
begin
    null;
end t257;
