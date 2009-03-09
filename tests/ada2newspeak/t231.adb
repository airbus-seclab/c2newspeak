-- case..when statement : "others =>" syntax
--
-- Author : Etienne Millon
-- Date   : Mon Mar  9 2009
--
procedure t231 is
    X : Integer := 3;
    Y : Integer;
begin
    Y := 1;
    case X is
        when    0   => Y := 0;
        when    2   => Y := 5;
        when others => Y := -2;
    end case;
    Y := 8;
end t231;
