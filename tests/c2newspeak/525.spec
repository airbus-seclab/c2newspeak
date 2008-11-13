Warning: comma in expression accepted in 525.c line 29
Warning: block within expression accepted in 525.c line 29
Warning: the order of execution of side-effects in expressions not specified, picking a random one, be careful in 525.c line 29
Newspeak output
---------------
525.c
main() {
  (525.c:27#6)^int32 x;
  (525.c:28#6)^int32 y;
  (525.c:29#2)^choose {
    | ! (1-_int32 ==_int32 0) -->
      (525.c:29#2)^1- =(int32) 1;
    | (1-_int32 ==_int32 0) -->
      (525.c:29#28)^0- =(int32) 1;
      (525.c:29#28)^{
        int32 tmp0;
        (525.c:29#28)^choose {
          | ! (2-_int32 ==_int32 0) -->
            (525.c:29#28)^0- =(int32) 1;
          | (2-_int32 ==_int32 0) -->
            (525.c:29#28)^0- =(int32) 0;
        }
        (525.c:29#2)^2- =(int32) 0-_int32;
      }
  }
}


