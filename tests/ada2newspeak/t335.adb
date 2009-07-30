-- 4.3.3 : The "others" clause shall be the last one.
--
-- This test should raise a compile-time error.
--
-- Author : Etienne Millon
-- Date   : Thu Jul 30 2009
--
procedure t335 is
  subtype Index is Integer range 0..9;
  type Int_array is array (Index) of Boolean;
  A : Int_array;
begin
    A := ( 5 => true, others => false, 2 => true);
end t335 ;
