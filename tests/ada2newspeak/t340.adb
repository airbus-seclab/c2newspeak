-- Chain of selected names (A.B.C).
--
-- Author : Etienne Millon
-- Date   : Thu Jul 30 2009
--
procedure t340 is
  type R1 is record
    Z : INTEGER;
  end record;
  type R2 is record
    Y : R1;
  end record;
  X : R2;
begin
  X.Y.Z := 1;
  null;
end t340 ;
