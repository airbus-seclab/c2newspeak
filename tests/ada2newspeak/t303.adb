-- Type compatibility between an integer constant and an integral type.
--
-- Author : Etienne Millon
-- Date   : Thu Jul  2 2009
--
procedure t303 is
  type New_Int is new Integer;
  X : New_Int;
  Y : constant := 2;
begin
    X := Y;
end t303 ;
