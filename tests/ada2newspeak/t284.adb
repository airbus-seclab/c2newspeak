-- Circular definition for renaming_declaration.
--
-- Author : Etienne Millon
-- Date   : Wed Jun 10 2009
--
procedure t284 is
  X : Integer renames X;
begin
    null;
end t284;
