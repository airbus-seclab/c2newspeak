-- Simple case of operator overloading.
--
-- Author : Etienne Millon
-- Date   : Tue May 19 2009
--
package body t261 is
  function "+" (Left : Integer ; Right : Integer) return Integer is
  begin
    return 0;
  end;

  procedure main is
    X : Integer;
  begin
    X := 2 + 3;
  end;
end t261;
