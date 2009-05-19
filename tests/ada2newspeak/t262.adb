-- Operator overloading : an operator name should be a builtin one.
--
-- Author : Etienne Millon
-- Date   : Tue May 19 2009
--
function "@!" (Left : Integer ; Right : Integer) return Integer is
  return 0;
end
