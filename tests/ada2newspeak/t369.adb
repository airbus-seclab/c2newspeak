-- in, in out and out parameter modes.
--
-- Author : Etienne Millon
-- Date   : Wed Aug 19 2009
--
package body t369 is
  procedure F (X : in Integer; Y : in out Integer; Z : out Integer) is
  begin
    Y := Y + 1;
    Z := 8;
  end;

  procedure main is
    U : Integer;
    V : Integer;
  begin
    U := 4;
    F (3, U, V);
  end;
end t369;
