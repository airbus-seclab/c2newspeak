-- Assignment from base type to derived type
--
-- Author : Etienne Millon
-- Date   : Thu Jul 23 2009
--
procedure t316 is
  type Int is new Integer;
  X : Integer;
  Y : Int;
begin
    Y := X;
end t316 ;
