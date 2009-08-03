-- 'Address attribute for the some element of an array.
--
-- Author : Etienne Millon
-- Date   : Mon Aug  3 2009
--
procedure t347 is
  subtype Index is Integer range 0..9;
  type Int_array is array (Index) of Boolean;
  A : Int_array;
  ADDR : System.Address;
begin
    ADDR := A'Address;
    ADDR := A(7)'Address;
end t347 ;
