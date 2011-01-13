package  T461 is
   subtype GE is Integer range 1 .. 2;
   type E is record
      B0: Integer ;
      B1:  Integer ;
   end record;
   type T_E is  array ( GE ) of E;
   function M (Y : in GE) Return  E;
   procedure A;
end T461;
