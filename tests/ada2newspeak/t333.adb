-- Array aggregates.
--
-- Author : Etienne Millon
-- Date   : Wed Jul 29 2009
--
procedure t333 is
  subtype Index is Integer range 0..9;
  type Int_array is array (Index) of Integer;
  A : Int_array;
begin
    A := ( 0 =>  0,
           1 =>  2,
           2 =>  4,
           3 =>  6,
           4 =>  8,
           5 => 10,
           6 => 12,
           7 => 14,
           8 => 16,
           9 => 18);
end t333;
