-- Lexer : integer literals (see RM95, 2.4)
-- each identifier denotes the expected value in decimal. "m" is for "-"
-- eg , N253 should be equal to 253, Nm2 to -2.
--
-- Etienne Millon
procedure t217 is
    N65    : Integer := 65;       -- classic
    N420   : Integer := 42E1;     -- positive exponent
    N63    : Integer := 63E0;     -- zero exponent
    N619   : Integer := 6_1_9;    -- underscores
    N5400  : Integer := 54E0_2;   -- underscore in exponent
    N48000 : Integer := 48e3;     -- small e
    Nm24   : Integer := -24;      -- negative
    N0     : Integer := -0;       -- zero with explicit minus
    N280   : Integer := 28E+1;    -- positive exponent with explicit plus
    N27    : Integer := 27E+0;    -- zero exponent with explicit plus
--    N58    : Integer := 58E-0;  -- zero exponent with explicit minus : skipped
begin
    null;
end t217;
