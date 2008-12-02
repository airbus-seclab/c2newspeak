Warning: 523.c:28#1075: block within expression accepted
Newspeak output
---------------
523.c
main() {
  (523.c:27#6)^int32 x;
  (523.c:28#2)^int32 tmp0;
  (523.c:28#2)^choose {
    | ! (1-_int32 ==_int32 0) -->
      (523.c:28#2)^0- =(int32) 1;
    | (1-_int32 ==_int32 0) -->
      (523.c:28#2)^0- =(int32) 0;
  }
  (523.c:28#2)^1- =(int32) 0-_int32;
}


