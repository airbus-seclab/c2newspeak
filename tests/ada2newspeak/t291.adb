-- Name resolution and hiding.
--
-- Here local X should hide package's X.
--
-- Author : Etienne Millon
-- Date   : Thu Jun 18 2009
--
package body t291 is
  procedure p is
    X : Integer;
    Y : Integer;
  begin
    Y := X;
  end;
end t291;
