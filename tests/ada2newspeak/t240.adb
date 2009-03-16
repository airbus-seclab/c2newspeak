-- Bugfix - everything after a "for" loop was discarded.
--
-- Author : Etienne Millon
-- Date   : Mon Mar 16 2009
--
procedure t240 is
    X : Integer;
begin
    for I in 4..8 loop
        X := 3;
    end loop;
    for J in reverse 12..16 loop
        X := 8;
    end loop;
    X := 58;
end t240;
