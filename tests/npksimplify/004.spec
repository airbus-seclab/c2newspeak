004.c
main() {
  (004.c:27#1072)^int32;
  (004.c:28#1084)^int32;
  (004.c:30#1092)^0- =(int32) 0;
  (004.c:32#1110)^0- =(int32) coerce[-2147483648,2147483647] (0 + 1);
  (004.c:33#1119)^1- =(int32) ! (10 > coerce[-2147483648,2147483647] (0 + 1));
  (004.c:31#1101)^do {
    (004.c:31#1101)^while (1) {
      (004.c:31#1101)^choose {
        | (0 ==_int32 1-_int32) -->
        | ! (0 ==_int32 1-_int32) -->
          (004.c:31#1101)^goto lbl2;
      }
      (004.c:32#1110)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
      (004.c:33#1119)^1- =(int32) ! (10 > 0-_int32);
    }
  } with lbl2: {
  }
}

