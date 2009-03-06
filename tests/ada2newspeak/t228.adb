-- Pragmas. Tests pragmas without arguments, or with a list of positional or
-- named arguments (expressions).
--
-- Author : Etienne Millon
-- Date   : Fri Mar  6 2009
--
procedure t228 is
    X : Integer := 2;
    pragma a_first_pragma;
    pragma another_pragma_with_arg        (1);
    pragma another_pragma_with_args       (X, 5);
    pragma another_pragma_with_named_args (first => 1, second => 5);
begin
    null;
end t228;
