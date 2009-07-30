-- Positional array aggregates.
--
-- Author : Etienne Millon
-- Date   : Thu Jul 30 2009
--
procedure t338 is
  subtype Index is Integer range 0..9;
  type Int_array is array (Index) of Boolean;
  A : Int_array;
begin
  A := ( true, false,
         true, false,
         true, false,
         true, false,
         true, false
  );
end t338 ;
