-- 4.3.3 : Only one "others" clause shall appear.
--
-- This test should raise a compile-time error.
--
-- Author : Etienne Millon
-- Date   : Thu Jul 30 2009
--
procedure t335 is
  subtype Index is Integer range 0..9;
  type Int_array is array (Index) of Integer;
  A : Int_array;
begin
    A := ( 5 => 1, others => 2, others => 3);
end t335 ;
