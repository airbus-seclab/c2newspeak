procedure t410 is
   type I  is ( T1, T2);
   type T_C is
      record
         D   : Integer;
         F : Integer;
      end record;

   type T_T is array (I) of T_C;

  Ci : constant  T_T := (  ( 1, 5 ),
                            ( 2, 8)
                       );


begin
   null;

end t410;
