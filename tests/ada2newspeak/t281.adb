-- Circular dependency between two packages.
-- Note that the (artificial) dependency is present in specification files too.
--
-- Author : Etienne Millon
-- Date   : Tue Jun  9 2009
--
with t281a;
package body t281 is
  procedure f is
  begin
    t281a.f;
  end;
end t281;
