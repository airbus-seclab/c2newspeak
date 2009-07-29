-- 'range attribute
--
-- Author : Etienne Millon
-- Date   : Wed Jul 29 2009
--
procedure t329 is
  subtype INDEX is Integer range 1..10;
  type FLOAT_ARRAY is array (INDEX) of FLOAT;
  A : FLOAT_ARRAY;
begin
    for I in A'RANGE loop
      null;
    end loop;
end t329;
