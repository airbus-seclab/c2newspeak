-- Error recovery.
--
-- There are two errors in this file : a missing ')' and a missing ';'.
-- Both should be reported.
--
-- Author : Etienne Millon
-- Date   : Thu Apr 23 2009
--
procedure t251 is
  function badly_defined (X:Integer;
begin
    null
end t251;
