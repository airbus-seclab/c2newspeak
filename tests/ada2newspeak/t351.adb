-- Qualified procedure call.
--
-- Author : Etienne Millon
-- Date   : Tue Aug  4 2009
--
package body t351 is
  procedure p is
  begin
    null;
  end;

  procedure main is
  begin
    t351.p;
  end;
end t351;
