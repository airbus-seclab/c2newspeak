-- Cycle in renaming declarations.
--
--    t279.p <--> t279a.p
--
-- Author : Etienne Millon
-- Date   : Tue Jun  9 2009
--
with t279a;
package body t279 is
  procedure main is
  begin
    p;
  end;
end t279;
