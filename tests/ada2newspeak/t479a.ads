Package T479a is
   subtype R is Integer range 1 .. 3;
   type toto is array (R) of Integer;
   type P is record
      NAV : toto;
   end record;
   DATA : P;
end t479a;
