procedure t457 is
   type T_INT8 is range -2 ** 7  .. 2 ** 7 - 1;
   type T_INT11 is range -2 ** 10  .. 2 ** 10 - 1;
   type T_INT3 is range -2 ** 2  .. 2 ** 2 - 1;
   type TF     is digits 6;
    for  TF'SIZE use 32;
    V  : TF;
    E : constant T_INT11:=T_INT11(T_INT8'LAST*8);
    E2 : constant T_INT3:=T_INT3(T_INT8'LAST*8);
    C : constant TF:=(TF'Last);
    D : constant TF:=TF(T_INT8'LAST*8);
   C1 : constant TF:=(TF'Last/TF(T_INT8'LAST*8));
   begin
        V := C1;
   end T457;

