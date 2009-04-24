-- Declarations requiring completion. (3.11.1)
--
-- Procedure g is declared, but requires completion by a body.
-- This test case should raise an error.
--
-- Author : Etienne Millon
-- Date   : Fri Apr 24 2009
--
package body t252 is
  procedure f is
  begin
    null;
  end;

  procedure g;
end t252;
