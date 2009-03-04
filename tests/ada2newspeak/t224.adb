-- Lexer, based decimal numerals (see RM95, 2.4.2)
--
-- An error should occur when a digit is >= base.
-- This test is for the digit > base case.
--
-- Author : Etienne Millon
-- Date   : Wed Mar  4 2009
--
procedure t224 is
    X : Integer := 13#24f0#; -- 'f' means 15 > 13
begin
    null;
end t224;
