-- Raw assignment to enum in initializer.
--
-- Author : Etienne Millon
-- Date   : Tue Jul 28 2009
--
procedure t322 is
  type E is (A, B);
  X : E := 1;
begin
    null;
end t322 ;
