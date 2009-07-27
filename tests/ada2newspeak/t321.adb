-- First and Last attributes for signed subtypes.
--
-- Author : Etienne Millon
-- Date   : Mon Jul 27 2009
--
procedure t321 is
  subtype T is INTEGER range 25..37;
  X : T;
begin
    X := T'First;
    X := T'Last;
end t321;
