-- Multiple renaming declaration are illegal.
--
-- Author : Etienne Millon
-- Date   : Wed Jun 10 2009
--
procedure t283 is
  X : Integer;
  Y, Z : Integer renames X;
begin
    null;
end t283;
