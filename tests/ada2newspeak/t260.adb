-- And/Then construction : the second argument may be evaluated, but only if the
-- first is false.
--
-- Author : Etienne Millon
-- Date   : Thu May  7 2009
--
procedure t260 (X : Boolean) is
  Y : constant Boolean := false and then X;
begin
    null;
end t260;
