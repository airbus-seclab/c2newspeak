-- Lexer : integer literals (see RM95, 2.4)
-- No trailing underscore in exponent part.
--
-- Etienne Millon
procedure t221 is
    X : Integer := 5E6_;
begin
    null;
end t221;
