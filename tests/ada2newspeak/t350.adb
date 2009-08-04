-- A(B).C, function case.
--
-- Author : Etienne Millon
-- Date   : Tue Aug  4 2009
--
package body t350 is
  function F (P : Integer) return R is
    Z : R;
  begin
    Z.X := P;
    Z.Y := 8;
    return Z;
  end;

  procedure main is
    A : Integer;
  begin
    A := F(0).X;
  end;
end t350;
