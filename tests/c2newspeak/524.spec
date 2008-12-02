Warning: 524.c:29#1084: comma in expression accepted
Warning: 524.c:29#1084: block within expression accepted
Warning: 524.c:29#24: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
524.c
main() {
  (524.c:27#6)^int32 x;
  (524.c:28#6)^int32 y;
  (524.c:29#24)^0- =(int32) 1;
  (524.c:29#24)^{
    int32 tmp0;
    (524.c:29#24)^choose {
      | ! (2-_int32 ==_int32 0) -->
        (524.c:29#24)^0- =(int32) 1;
      | (2-_int32 ==_int32 0) -->
        (524.c:29#24)^0- =(int32) 0;
    }
    (524.c:29#2)^2- =(int32) 0-_int32;
  }
}


