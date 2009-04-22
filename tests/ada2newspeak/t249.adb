-- When a package body is provided, the specification should be checked.
-- Here the .ads file claims that the implementation shall provide
-- a procedure g, which does not exist.
--
-- Author : Etienne Millon
-- Date   : Tue Apr 21 2009
--
package body t249 is
  procedure f is
  begin
    null;
  end;
end t249;
