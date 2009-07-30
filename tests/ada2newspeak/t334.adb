-- Array aggregate with "others"
--
-- Author : Etienne Millon
-- Date   : Thu Jul 30 2009
--
procedure t334 is
  subtype Index is Integer range 0..9;
  type Int_array is array (Index) of Boolean;
  A : Int_array;
begin
    A := ( 5 => true, others => false);
end t334 ;
