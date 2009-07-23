-- Raw assignment to an enumeration type.
--
-- Author : Etienne Millon
-- Date   : Thu Jul 23 2009
--
procedure t315 is
  type E is (A, B);
  X : E;
begin
    X := 1;
end t315 ;
