-- Weird bug (20..200 works but not 21..200)
--
-- Author : Etienne Millon
-- Date   : Mon Jul 20 2009
--
procedure t310 is
   type R is range 21..200;
   Z : R;
begin
    null;
end t310 ;
