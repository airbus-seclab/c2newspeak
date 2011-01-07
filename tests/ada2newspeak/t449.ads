package  T449 is
    type T_2  is record
      A       : Integer;
      B       : Integer;
   end record;
   type T_3  is record
      E       : Integer;
      F       : T_2;
   end record;
   procedure Gg;
end T449;
