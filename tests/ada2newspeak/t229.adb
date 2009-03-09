-- Case .. when coumpound statement.
--
-- Simple test case with constant values.
--
-- Author : Etienne Millon
-- Date   : Mon Mar  9 2009
--
procedure t229 is
    X : Integer := 0;
begin
    case 3 is
        when 32 => X := 3; X := 2;
        when 45 => X := 4; X := 5;
        when 58 => X := 5; X := 8;
        when 78 => X := 7; X := 8;
    end case;
end t229;
