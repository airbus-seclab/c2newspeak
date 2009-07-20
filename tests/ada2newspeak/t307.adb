-- "succ" function-attribute for scalar types.
--
-- Author : Etienne Millon
-- Date   : Mon Jul 20 2009
--
procedure t307 is
  type Enum is (A, B, C);
  X : Enum;
begin
    X := B;
    X := Enum'Succ (A);
    X := C;
    X := Enum'Succ (B);
end t307 ;
