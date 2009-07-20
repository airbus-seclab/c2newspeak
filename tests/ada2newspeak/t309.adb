-- Range of signed types.
--
-- Author : Etienne Millon
-- Date   : Mon Jul 20 2009
--
procedure t309 is
  type t1 is range    0..0;    x1 : t1;
  type t2 is range   -2..1;    x2 : t2;
  type t3 is range   -4..3;    x3 : t3;
  type t4 is range   -8..7;    x4 : t4;
  type t5 is range  -16..15;   x5 : t5;
  type t6 is range  -32..31;   x6 : t6;
  type t7 is range  -64..63;   x7 : t7;
  type t8 is range -128..127;  x8 : t8;
  type t9 is range -256..255;  x9 : t9;
begin
    null;
end t309 ;
