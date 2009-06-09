-- Circular dependency between two packages.
--
-- Author : Etienne Millon
-- Date   : Tue Jun  9 2009
--
with t281;
package body t281a is
  procedure f is
  begin
    t281.f;
  end;
end t281a;
