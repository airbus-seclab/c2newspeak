-- "pred" function-attribute for enumerated types.
--
-- Author : Etienne Millon
-- Date   : Wed Jul 29 2009
--
procedure t327 is
  type Enum is (A, B, C);
  X : Enum;
begin
    X := Enum'Pred (B);
    X := A;
    X := Enum'Pred (C);
    X := B;
    X := Enum'Pred (A);
end t327;
