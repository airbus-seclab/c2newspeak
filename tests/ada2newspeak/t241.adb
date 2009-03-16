-- Named and optional parameters.
--
-- Author : Etienne Millon
-- Date   : Mon Mar 16 2009
--
procedure t241 (X : Integer;
                Y : Integer := 1;
                Z : Integer := 9) is
begin
    t241(6, 1, 9);           -- all positional (C style)
    t241(X => 6,             -- all named
         Y => 1,
         Z => 9);
    t241(Y => 1,             -- all named, not in order
         X => 6,
         Z => 6);
    t241(6, Y => 1, Z => 9); -- mixed, order
    t241(6, Z => 9, Y => 1); -- mixed, not in order
    t241(6, Y => 1);         -- with default value for Z
    t241(6);                 -- with default value for Y and Z
end t241;
