-- Compile-time "and" and "or" (no shortcircuit).
--
-- Author : Etienne Millon
-- Date   : Mon Jun 29 2009
--
procedure t294 is
  Xt1 : constant Boolean := true  and true ;
  Xf2 : constant Boolean := true  and false;
  Xf3 : constant Boolean := false and true ;
  Xf4 : constant Boolean := false and false;
  Yt1 : constant Boolean := true  or  true ;
  Yt2 : constant Boolean := true  or  false;
  Yt3 : constant Boolean := false or  true ;
  Yf4 : constant Boolean := false or  false;
begin
    null;
end t294 ;
