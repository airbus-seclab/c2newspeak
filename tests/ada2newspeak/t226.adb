-- Lexer, based decimal numerals (see RM95, 2.4.2)
-- 
-- (2.4.2.6) : "The base (the numeric value of the decimal numeral preceding the first #)
--              shall be at least two and at most sixteen."
--
-- This test is for the "at least" part.
--
-- Author : Etienne Millon
-- Date   : Wed Mar  4 2009
-- 
procedure t226 is
    X : Integer := 1#0#; -- base 1 < 2
begin
    null;
end t226;
