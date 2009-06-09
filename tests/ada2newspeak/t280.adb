-- Cycle in renaming declarations.
--
--    t279.p <--> t279a.p
--
-- This is essentially the same as t279, but without the "with t279a"
-- in the main package.
-- It behave like t279, because t279a should be "with"ed implicitly.
--
-- Author : Etienne Millon
-- Date   : Tue Jun  9 2009
--
package body t280 is
  procedure main is
  begin
    p;
  end;
end t280;
