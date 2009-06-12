-- Parameterless function call.
--
-- Author : Etienne Millon
-- Date   : Fri Jun 12 2009
--
package body t286 is
  function F return Integer is
  begin
    return 0;
  end;

  procedure main is
    X : Integer;
  begin
    X := F;
  end;
end t286;
