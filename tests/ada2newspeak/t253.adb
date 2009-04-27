-- 6.1.(18) : "The formal parameters of a function,
--             if any, shall have the mode In."
--
-- This function has a parameter with mode Out, and shall raise an error.
--
-- Author : Etienne Millon
-- Date   : Mon Apr 27 2009
--
function t253(X : out Integer) return Integer is
begin
  return 0;
end t253;
