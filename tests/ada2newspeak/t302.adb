-- Access to a record type.
--
-- Author : Etienne Millon
-- Date   : Tue Jun 30 2009
--
procedure t302 is
  type Container is record
      Value : Integer;
    end record;
  X : Container;
begin
  X.Value := 24;
end t302 ;
