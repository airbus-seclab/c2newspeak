--
--
-- Author : Etienne Millon
-- Date   : Wed Apr 22 2009
--
package t250 is
  type N is (A, B, C);
  procedure f;
  function "+" (Left, Right : N) return N;
end t250;
