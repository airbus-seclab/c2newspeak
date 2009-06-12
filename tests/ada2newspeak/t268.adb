-- Operator name in dotted name.
--
-- Author : Etienne Millon
-- Date   : Mon Jun  8 2009
--
package body t268 is
  function "+" (Left : Integer ; Right : Integer) return Integer is
  begin
    return 0;
  end;

  procedure main is
    X : Integer;
  begin
    X := t268."+" (Left => 0, Right => 0);
  end;
end t268;
