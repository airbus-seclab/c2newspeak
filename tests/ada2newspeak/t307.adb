-- "succ" function-attribute for enumerated types.
--
-- Author : Etienne Millon
-- Date   : Mon Jul 20 2009
--
procedure t307 is
  type Enum is (A, B, C);
  X : Enum;
begin
    X := Enum'Succ (A);
    X := B;
    X := Enum'Succ (B);
    X := C;
    X := Enum'Succ (C);
end t307 ;
