-- 6.1.(19) : "A default_expression is only allowed in a parameter_specification
--             for a formal parameter of mode in."
--
-- So, the following program (with a default value
-- for an Out parameter) is illegal.
--
-- Author : Etienne Millon
-- Date   : Mon Apr 27 2009
--
procedure t256 (X : in out Integer := 2) is
begin
    null;
end t256;
