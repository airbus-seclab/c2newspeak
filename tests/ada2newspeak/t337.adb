-- For I in subtype_name syntax.
--
-- Author : Etienne Millon
-- Date   : Thu Jul 30 2009
--
procedure t337 is
    subtype INDEX is INTEGER range 0..10;
begin
    for I in INDEX loop
      null;
    end loop;
end t337 ;
