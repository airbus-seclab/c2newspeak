-- Lexer : integer literals (see RM95, 2.4)
-- Multiple underscores are not allowed.
--
-- Etienne Millon
procedure t222 is
    X : Integer := 5__6;
begin
    null;
end t222;
