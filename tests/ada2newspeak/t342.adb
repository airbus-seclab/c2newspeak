-- Use of aggregates in initializer.
--
-- Author : Etienne Millon
-- Date   : Fri Jul 31 2009
--
procedure t342 is
  subtype Index is Integer range 0..9;
  type Int_array is array (Index) of Boolean;
  A : Int_array := (1 => true, others => false);
begin
    null;
end t342 ;
