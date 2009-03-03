-- Lexer : integer literals (see RM95, 2.4)
-- No trailing underscore in main part.
--
-- Etienne Millon
procedure t219 is
    X : Integer := 5_;
begin
    null;
end t219;
