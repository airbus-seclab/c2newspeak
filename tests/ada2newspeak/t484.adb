procedure T484 is
   subtype T_2 is Integer range 1 .. 2;
    subtype T_4 is Integer range 1 .. 4;
   type T is array ( T_2 , T_4) of Integer ;
   A : T;
begin
   A := ( 1 => (
                1 => 1,
                2 => 2,
                3 => 3,
                4 => 4
                ),
             2 => (
                   1 => 5,
                   2 => 6,
                   3 => 7,
                   4 => 8
                  )
         );
end T484;
