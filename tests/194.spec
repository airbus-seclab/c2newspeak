Newspeak output
---------------
194.c
main() {
  (194.c:27#1073)^int32 x;
  (194.c:29#1081)^int32 tmp0;
  (194.c:29#1081)^choose {
    | ! (1-_int32 ==_int32 0) -->
      (194.c:29#1081)^choose {
        | ! (1-_int32 ==_int32 0) -->
          (194.c:29#1081)^0- =(int32) 1-_int32;
        | (1-_int32 ==_int32 0) -->
          (194.c:29#1081)^0- =(int32) 0;
      }
    | (1-_int32 ==_int32 0) -->
      (194.c:29#1081)^0- =(int32) 0;
  }
  (194.c:29#1081)^1- =(int32) 0-_int32;
}


