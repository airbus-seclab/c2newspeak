Warning: increment statement expected in 313.c line 30
Newspeak output
---------------
313.c
main() {
  (313.c:27#1076)^int32 i;
  (313.c:28#1085)^0- =(int32) 0;
  (313.c:28#1081)^do {
    (313.c:28#1081)^while (1) {
      (313.c:28#1081)^choose {
        | ! (0-_int32 > 10) -->
        | (0-_int32 > 10) -->
          (313.c:28#1081)^goto lbl2;
      }
      (313.c:29#1109)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl2: {
  }
}


