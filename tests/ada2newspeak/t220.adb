-- Lexer : integer literals (see RM95, 2.4)
-- No leading underscore in exponent part.
--
-- Etienne Millon
procedure t220 is
    X : Integer := 5E_6;
begin
    null;
end t220;
