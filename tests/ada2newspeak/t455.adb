procedure t455 is
   type T_INT8 is range -2 ** 7  .. 2 ** 7 - 1;
   V   : FLOAT;
   C3  : constant FLOAT:=(FLOAT'LAST);
   C2  : constant FLOAT:=FLOAT(T_INT8'LAST*8);
   C1  : constant FLOAT:=(FLOAT'Last/FLOAT(T_INT8'LAST*8));
   begin
        V := C3;
        V := C1;
        V := C2;

end T455;
