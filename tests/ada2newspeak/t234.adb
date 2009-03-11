-- Affectation and comparison of the built-in Integer type.
--
-- Author : Etienne Millon
-- Date   : Wed Mar 11 2009
--
procedure t234 is
    I, J, K : Integer;
    R       : Boolean;
begin
    -- Integer litterals

    I :=    0;
    I :=  500;
    I := -500;

    -- Operators

    -- "="
    R := (I =  J);

    -- "/="
    R := (I /= J);

    -- "<"
    R := (I <  J);

    -- "<="
    R := (I <= J);

    -- ">"
    R := (I >  J);

    -- ">="
    R := (I >= J);

    -- binary "+"
    K := I + J;

    -- binary "-"
    K := I - J;

    -- unary "+"
    K := +J;

    -- unary "-"
    K := -J;

    -- "abs" TODO
--  K := abs J;

    -- "**" TODO
--  K := I ** J;

    -- "*"
    K := I * J;

    -- "/"
    K := I / J;

    -- "mod" TODO
--  K := I mod J;

    -- "rem"
    K := I rem J;

    -- "&" is defined only for non-limited, one-dimensional arrays.

    -- "not" is defined for boolean types, modular types, and one-dimensional
    -- array types whose components are of a boolean type only.

    -- "and", "or" and "xor" are only defined for boolean types, modular types,
    -- and one-dimensional array types whose components are of a boolean type.
    
end t234;
