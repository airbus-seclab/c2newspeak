-- Instanciation of a record with different sizes.
--
-- Author : Etienne Millon
-- Date   : Tue Jun 30 2009
--
procedure t301 is
  type R is record
    F1 : Integer;
    F2 : Boolean;
  end record;
  X : R;
begin
    null;
end t301 ;
