procedure t074 is
   X, Y : Integer;
   subtype T1 is Integer range 0..10;
   subtype T2 is T1 range 2..7;
   subtype T3 is T2 range 3..7;
   A : constant T3 := 4;
   N : constant := A*2;
begin
   X := A;
   Y := 10 + N;

end t074;
