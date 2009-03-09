-- Case .. when coumpound statement.
--
-- Simple test case with constant values.
--
-- Author : Etienne Millon
-- Date   : Mon Mar  9 2009
--
procedure t229 is
    X : Integer range 3..6 := 3;
    Y : Integer;
begin
    case X is
        when 3 => Y := 3; Y := 2;
        when 4 => Y := 4; Y := 5;
        when 5 => Y := 5; Y := 8;
        when 6 => Y := 7; Y := 8;
    end case;
end t229;
