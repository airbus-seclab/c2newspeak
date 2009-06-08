-- 6.1.(21) : "A name that denotes a formal parameter is not allowed
--             within the formal_part in which it is declared [...]"
--
-- The default_parameter provided for Y should raise an error.
--
-- Author : Etienne Millon
-- Date   : Fri Jun  5 2009
--
procedure t264 (X : Integer ; Y : Integer := X) is
begin
    null;
end t264;
