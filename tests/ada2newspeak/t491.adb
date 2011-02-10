procedure T491 is
   subtype T_2 is Integer range 1 .. 2;
   type TR is  array ( T_2 ) of Integer;
   type T is record
      Y : Integer;
      TRR  :  TR;
   end record;

   A  : T;

begin
   A := (
           Y => 2,
           TRR => (2,4)
          );

end T491;
