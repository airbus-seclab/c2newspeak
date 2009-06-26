-- Name resolution : subprogram parameters shall hide package-wide definitions.
--
-- Author : Etienne Millon
-- Date   : Thu Jun 25 2009
--
package body t292 is
  procedure f (X : T) is
    B : boolean;
  begin
    B := (X = Y);
  end;
end t292;
