Newspeak output
---------------
796.c
void main(void) {
  (796.c:2#24)^int32 i;
  int32 tmp;
  (796.c:3#29)^0- =(int32) 1-_int32;
  (796.c:3#29)^1- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 1);
  (796.c:3#29)^choose {
   -->
    (796.c:3#29)^guard(! (0-_int32 ==_int32 0));
   -->
    (796.c:3#29)^guard((0-_int32 ==_int32 0));
  }
}


