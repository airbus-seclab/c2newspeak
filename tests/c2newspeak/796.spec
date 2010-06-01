Warning: 796.c:3#2: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
796.c
void main(void) {
  (796.c:2#6)^int32 i;
  (796.c:3#2)^int32 .tmp;
  (796.c:3#2)^0- =(int32) ! (1-_int32 ==_int32 0);
  (796.c:3#2)^1- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 1);
  (796.c:3#2)^{
    int32 tmp0;
    (796.c:3#2)^choose {
     -->
      (796.c:3#2)^guard(1-_int32);
      (796.c:3#2)^0- =(int32) 1-_int32;
     -->
      (796.c:3#2)^guard(! 1-_int32);
      (796.c:3#2)^0- =(int32) 0;
    }
  }
}


