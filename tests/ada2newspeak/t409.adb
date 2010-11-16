procedure t409 is
   type T  is (N, T1, T2, A);
   subtype I is T range T1 .. T2;
   type T_T is array (I) of Integer;
   C : constant  T_T := ( 1, 1 );

begin
   null;

end t409;
