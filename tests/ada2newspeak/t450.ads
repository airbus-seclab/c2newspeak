package  T450 is
   subtype T is Integer range 1 .. 5;
   type T_3  is record
      E       : T;
      F       : T;
   end record;
   procedure Gg;
end T450;
