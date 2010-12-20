Warning: 796.c:3#2: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
void main(void) {
  (796.c:2#6)^int32 i;
  (796.c:3#2)^int32 tmp_parser!1;
  (796.c:3#2)^tmp_parser!1 =(int32) ! (i_int32 ==_int32 0);
  (796.c:3#2)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
  (796.c:3#2)^{
    int32 tmp_firstpass!0;
    (796.c:3#2)^choose {
     -->
      (796.c:3#2)^guard(tmp_parser!1_int32);
      (796.c:3#2)^tmp_firstpass!0 =(int32) tmp_parser!1_int32;
     -->
      (796.c:3#2)^guard(! tmp_parser!1_int32);
      (796.c:3#2)^tmp_firstpass!0 =(int32) 0;
    }
  }
}


