package T481a is
   subtype R is Integer range 1 .. 3;
   type toto is array (R) of Integer;
   type toto2 is record
      NA0 : Integer;
      NA : toto;
   end record;

   type P is record
      NAV0 : Integer;
      NAV1 : toto2;
      NAV2 : integer;
   end record;

end t481a;
