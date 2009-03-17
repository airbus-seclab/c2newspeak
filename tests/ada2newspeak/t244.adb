-- In a subprogram call, named parameters shall follow positional ones.
--
-- Author : Etienne Millon
-- Date   : Tue Mar 17 2009
--
procedure t244(X : Integer := 8;
               Y : Integer := 1;
               Z : Integer := 3) is
begin
    t244(5, Y => 2, 9);
end t244;
