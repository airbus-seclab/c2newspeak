Newspeak output
---------------
010.c
main() {
  (010.c:33#1224)^int32 x;
  (010.c:34#1229)^0- =(int32) 0;
  (010.c:35#1238)^do {
    (010.c:35#1238)^while (1) {
      (010.c:35#1238)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (010.c:35#1238)^goto lbl2;
      }
      (010.c:36#1259)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl2: {
  }
}

