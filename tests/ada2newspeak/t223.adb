-- Lexer, based decimal numerals (see RM95, 2.4.2)
--
-- Various valid constructions.
--
-- Author : Etienne Millon
-- Date   : Wed Mar  4 2009
--
procedure t223 is
    N51966      : Integer := 16#cafe#;      -- hexadecimal
    N14         : Integer := 2#1110#;       -- binary
    N98         : Integer := 8#142#;        -- octal
    N547        : Integer := 13#331#;       -- base 13
    Nm23        : Integer := -16#17#;       -- negative
    N255        : Integer := 1_6#ff#;       -- underscore in base
    N170        : Integer := 2#1010_1010#;  -- underscore in main part
    N47806      : Integer := 16#BABE#;      -- uppercase
    N26520      : Integer := 13#c0C0#;      -- mixed
    N11259375   : Integer := 16#ABCDEF#;    -- check all digits (uppercase letters)
    N11259375_2 : Integer := 16#abcdef#;    -- check all digits (lowercase letters)
    N74565      : Integer := 16#12345#;     -- check all digits (numbers part 1)
    N424080     : Integer := 16#67890#;     -- check all digits (numbers part 2)
    N154880     : Integer := 16#25d#2;      -- exponent
    N2048       : Integer := 2#1#1_1;       -- underscore in exponent
begin
    null;
end t223;
