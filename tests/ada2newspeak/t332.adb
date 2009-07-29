-- System.address
--
-- Author : Etienne Millon
-- Date   : Wed Jul 29 2009
--
with System;
procedure t332 is
  Addr : System.Address;
  X : Integer;
  Y : Float;
begin
    Addr := X'Address;
    Addr := Y'Address;
end t332 ;
