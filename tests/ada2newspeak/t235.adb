-- "For .. in .. loop" construct. The iterator is not read nor written
-- from inside the "loop" block.
--
-- Author : Etienne Millon
-- Date   : Thu Mar 12 2009
--
procedure t235 is
    X : Integer;
begin
    for I in 5..10 loop
        X := 1;
    end loop;
end t235;
