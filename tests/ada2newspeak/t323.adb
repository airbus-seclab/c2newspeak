-- Read access to a record type
--
-- Author : Etienne Millon
-- Date   : Tue Jul 28 2009
--
procedure t323 is
  type Container is record
      Value : Integer;
    end record;
  X : Container;
  Y : Integer;
begin
    Y := X.Value;
end t323 ;
