-- Lexer, based decimal numerals (see RM95, 2.4.2)
--
-- An error should occur when a digit is >= base.
-- This test is for the digit = base case.
--
-- Author : Etienne Millon
-- Date   : Wed Mar  4 2009
--
procedure t225 is
    X : Integer := 11#5b0#; -- 'b' means 11 = base
begin
    null;
end t225;
