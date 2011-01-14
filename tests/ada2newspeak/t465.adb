procedure T465 is
   type T_UINT16  is range 0 .. 2**16 - 1;
   C_M : constant T_UINT16 := 32;
   subtype TM is T_UINT16 range 1 .. C_M;
   type RD is record
     A : Integer;
     B : Integer;
   end record;


   type TMT     is  array  ( TM ) of  RD ;
   C  : constant  T_UINT16   :=24;
    MS   : T_UINT16:=0;

    M :  TMT ;

    --            --------
    subtype GE is T_UINT16 range 1 .. 2;
    --            --------
   type T is array ( GE ) of Integer;
   A : T;
begin
   for I in A'Range  loop
      if (I < A'Last) then
         null;
      end if;
   end loop ;

end T465;

