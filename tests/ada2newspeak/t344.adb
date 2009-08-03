-- Use of range expression in an aggregate.
--
-- Author : Etienne Millon
-- Date   : Fri Jul 31 2009
--
procedure t344 is
  subtype Index is Integer range 0..9;
  type Int_array is array (Index) of Boolean;
  A : Int_array := (2..6 => true, others => false);
begin
    null;
end t344 ;
