-- Function call in a static context.
-- This should fallback to a runtime computing.
--
-- Author : Etienne Millon
-- Date   : Thu May  7 2009
--
function t259 (P : Integer) return Integer is
  X : constant Integer := t259(5);
begin
  return 4;
end t259;
