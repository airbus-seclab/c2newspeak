-- Simple example for operator overloading.
--
-- The N type has a "+" operator for modular addition.
--
-- Author : Etienne Millon
-- Date   : Wed Apr 22 2009
--
package body t250 is
  function "+" (Left, Right : N) return N is
  begin
    case Left is
      when A => case Right is when A => return B;
                              when B => return C;
                              when C => return A;
                end case;

      when B => case Right is when A => return C;
                              when B => return A;
                              when C => return B;
                end case;

      when C => case Right is when A => return A;
                              when B => return B;
                              when C => return C;
                end case;
    end case;

  end;

  procedure f is
    X, Y : N := A;
    Z : N;
  begin
    Z := X + Y;
  end;

end t250;
